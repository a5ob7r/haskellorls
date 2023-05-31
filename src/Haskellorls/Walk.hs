{-# LANGUAGE TemplateHaskell #-}

module Haskellorls.Walk
  ( mkInitialOperations,
    LsState (..),
    LsConf (..),
    runLs,
    run,
  )
where

import Control.Exception.Safe (MonadCatch, MonadThrow, SomeException, toException, tryIO)
import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)
import Control.Monad.State.Strict (MonadState, StateT (..), runState)
import Data.Bifunctor (bimap)
import Data.ByteString (fromFilePath)
import Data.Default.Class (Default (..))
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import Data.List (foldl', intersperse, partition)
import Data.Sequence (Seq (Empty, (:<|)), fromList, singleton, (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import GHC.IO.Exception (IOErrorType (NoSuchThing), IOException (ioe_type))
import Haskellorls.Class (notify)
import Haskellorls.Config qualified as Config
import Haskellorls.Config.Format qualified as Format
import Haskellorls.Config.Tree
import Haskellorls.Data.Infinitable
import Haskellorls.Formatter qualified as Formatter
import Haskellorls.Formatter.Attribute qualified as Attr
import Haskellorls.Formatter.Layout.Grid qualified as Grid
import Haskellorls.Formatter.Quote qualified as Quote
import Haskellorls.Formatter.WrappedText qualified as WT
import Haskellorls.NodeInfo qualified as Node
import Haskellorls.System.OsPath.Posix.Extra (PosixPath, decode, encode, (</>))
import Haskellorls.Walk.Dired qualified as Dired
import Haskellorls.Walk.Listing qualified as Listing
import Haskellorls.Walk.Sort qualified as Sort
import Haskellorls.Walk.Utils qualified as Walk
import Lens.Micro.Mtl (use, view, (%=), (.=))
import Lens.Micro.TH (makeLenses)
import System.IO.Unsafe (unsafePerformIO)
import Witch (from, via)

data LsConf = LsConf
  { _configL :: Config.Config,
    printers :: Formatter.Printers
  }

makeLenses ''LsConf

data LsState = LsState
  { _inodesL :: Walk.Inodes,
    _indicesL :: Dired.NameIndeces,
    _errorsL :: [SomeException]
  }

instance Default LsState where
  def = LsState mempty Dired.empty []

makeLenses ''LsState

-- | A central piece monad of haskellorls.
--
-- NOTE: This is inspired from XMonad.
-- <https://github.com/xmonad/xmonad/blob/a902fefaf1f27f1a21dc35ece15e7dbb573f3d95/src/XMonad/Core.hs#L158>
newtype Ls a = Ls (ReaderT LsConf (StateT LsState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader LsConf, MonadState LsState)

runLs :: LsConf -> LsState -> Ls a -> IO (a, LsState)
runLs conf st (Ls a) = runStateT (runReaderT a conf) st

data EntryType = FILES | SINGLEDIR | DIRECTORY

data Entry = Entry
  { entryType :: EntryType,
    entryPath :: PosixPath,
    entryNodes :: [Node.NodeInfo],
    entryDepth :: Infinitable Int
  }

newtype Tree = Tree {treeNodes :: Seq.Seq Node.NodeInfo}

data Operation
  = Newline
  | PrintEntry Entry
  | PrintTree Tree

run :: LsConf -> LsState -> [Operation] -> IO ([Operation], LsState)
run conf st operations = runLs conf st $ go operations
  where
    go [] = do
      config <- view configL

      ([] <$) . when (Config.dired config) $ do
        indices <- use indicesL

        unless (null $ Dired.dired indices) $
          liftIO . T.putStrLn . TL.toStrict . TL.toLazyText $
            "//DIRED//" <> foldl' (\acc (x, y) -> foldr (\t acc' -> TL.fromText t <> acc') acc [" ", T.pack (show x), " ", T.pack (show y)]) mempty (Dired.dired indices)

        unless (null $ Dired.subdired indices) $
          liftIO . T.putStrLn . TL.toStrict . TL.toLazyText $
            "//SUBDIRED//" <> foldl' (\acc (x, y) -> foldr (\t acc' -> TL.fromText t <> acc') acc [" ", T.pack (show x), " ", T.pack (show y)]) mempty (Dired.subdired indices)

        liftIO . T.putStrLn $
          "//DIRED-OPTIONS// --quoting-style="
            <> case Config.quotingStyle config of
              Quote.Literal -> "literal"
              Quote.Shell -> "shell"
              Quote.ShellAlways -> "shell-always"
              Quote.ShellEscape -> "shell-escape"
              Quote.ShellEscapeAlways -> "shell-escape-always"
              Quote.C -> "c"
              Quote.Escape -> "escape"
              Quote.CLocale _ _ -> "clocale"
              Quote.Locale _ _ -> "locale"
    go (op : ops) = do
      LsConf config printers <- ask

      let f acc d = do
            liftIO . T.putStr . from $ Attr.unwrap d

            return $!
              if Config.dired config
                then Dired.update (unsafePerformIO . fromFilePath . T.unpack . WT.wtWord <$> d) acc
                else acc
          divs = mkDivisions config printers op
       in use indicesL >>= \indices -> foldM f indices divs >>= (indicesL .=)

      case op of
        PrintEntry Entry {..} | Config.recursive config && entryDepth < Config.level config -> do
          (nodes, inodes) <- do
            let nodes = filter (maybe False Node.isDirectory . Node.nodeType) entryNodes
            runState (Walk.filterNodes nodes) <$> use inodesL
          inodesL .= inodes

          entries <-
            foldr (\entry acc -> Newline : entry : acc) []
              <$> mkOperations config (succ <$> entryDepth) ((\node -> entryPath </> Node.getNodePath node) <$> nodes)

          go $ entries <> ops
        _ -> go ops

-- NOTE: In this application, and probably almost all applications, traversing
-- directory contents and getting each file statuses are non-atomic. So maybe
-- causes looking up nonexistence filepaths. In such case, ignore nonexistence
-- filepaths.
ignorable :: Bool -> IOException -> Bool
ignorable onCommandLine e = not onCommandLine && ioe_type e == NoSuchThing

mkNodes :: Bool -> Config.Config -> PosixPath -> [PosixPath] -> Ls [Node.NodeInfo]
mkNodes onCommandLine config parent paths = do
  (errs, nodes) <-
    bimap (foldr (\e acc -> if ignorable onCommandLine e then acc else toException e : acc) []) (Sort.sort config) . partitionEithers
      <$> mapM (tryIO . Node.mkNodeInfo config parent) paths

  errorsL %= (errs <>)
  mapM_ notify errs

  return nodes

mkOperations :: Config.Config -> Infinitable Int -> [PosixPath] -> Ls [Operation]
mkOperations _ _ [] = return []
mkOperations config depth (parent : parents) =
  tryIO (Listing.listContents config parent) >>= \case
    Left e -> do
      unless (ignorable False e) $ notify e >> errorsL %= (toException e :)
      mkOperations config depth parents
    Right paths -> do
      nodes <- mkNodes False config parent paths
      (PrintEntry (Entry DIRECTORY parent nodes depth) :) <$> mkOperations config depth parents

mkInitialOperations :: [PosixPath] -> Ls [Operation]
mkInitialOperations paths = do
  config <- view configL
  nodes <- do
    nodes <- mkNodes True config (encode "") paths
    (_, inodes) <- runState (Walk.filterNodes nodes) <$> use inodesL
    inodesL .= inodes
    return nodes

  let depth = Only 1
      isDirectory
        | Config.directory config = const False
        | otherwise = maybe False Node.isDirectory . Node.nodeType
      (dirs, files) = partition isDirectory nodes
      fileOp = [PrintEntry (Entry FILES (encode "") files depth) | not $ null files]

  dirOps <-
    if Config.tree config
      then mapM (fmap (PrintTree . Tree) . mkTreeNodeInfos) dirs
      else
        mkOperations (Config.disableDereferenceOnCommandLine config) depth (Node.getNodePath <$> dirs) <&> \case
          -- Considers no argument to be also a single directory.
          [PrintEntry e] | null files && length paths < 2 -> [PrintEntry (e {entryType = SINGLEDIR})]
          ops -> ops

  return . intersperse Newline $ fileOp <> dirOps

mkTreeNodeInfos :: Node.NodeInfo -> Ls (Seq Node.NodeInfo)
mkTreeNodeInfos nodeinfo = do
  inodesL %= \inodes -> maybe inodes (\inode -> Walk.singleton inode <> inodes) $ Node.fileID nodeinfo

  go (singleton nodeinfo) mempty
  where
    go :: Seq Node.NodeInfo -> Seq Node.NodeInfo -> Ls (Seq Node.NodeInfo)
    go Empty nodes = return nodes
    go (node :<| nodeSeq) nodeSeq' = do
      config <- view configL
      let path = Node.getNodeDirName node </> Node.getNodePath node

      contents <-
        if Config.level config <= (Only . length $ Node.getTreeNodePositions node) || not (maybe False Node.isDirectory $ Node.nodeType node)
          then return []
          else
            tryIO (Listing.listContents config path) >>= \case
              Left e -> notify e >> errorsL %= (toException e :) >> return []
              Right contents -> return contents

      nodes <- do
        (nodes, inodes) <- do
          nodes <- mkNodes False config path contents
          runState (Walk.filterNodes nodes) <$> use inodesL
        inodesL .= inodes
        let pList = mkPositions (length nodes) $ Node.getTreeNodePositions node
        return $ zipWith (\nd p -> nd {Node.getTreeNodePositions = p}) nodes pList

      go (fromList nodes <> nodeSeq) (nodeSeq' |> node)

mkPositions :: Int -> [TreeNodePosition] -> [[TreeNodePosition]]
mkPositions n _ | n < 1 = [[]]
mkPositions 1 xs = [LAST : xs]
mkPositions n xs = case replicate n xs of
  [] -> []
  (y : ys) -> (HEAD : y) : go ys
  where
    go [] = []
    go [y] = [LAST : y]
    go (y : ys) = (MID : y) : go ys

mkDivisions :: Config.Config -> Formatter.Printers -> Operation -> [Attr.Attribute WT.WrappedText]
mkDivisions config printers =
  foldr (\x acc -> [Attr.Other $ from @T.Text "  " | Config.dired config && not (null x)] <> x <> [Attr.Other . from @T.Text $ if Config.zero config then "\0" else "\n"] <> acc) mempty . \case
    Newline -> [[]]
    PrintEntry (Entry {..}) ->
      let nodes = Formatter.mkLines entryNodes config printers $ Formatter.mkPrinterTypes config

          header = case entryType of
            FILES -> []
            SINGLEDIR | not (Config.recursive config) -> []
            _ -> [Attr.Dir . via @T.Text $ decode entryPath, Attr.Other $ from @T.Text ":"]

          -- Add total block size header only about directries when long style
          -- layout or @-s / --size@ is passed.
          totalBlockSize
            | Config.size config = Formatter.mkBlockSizeHeader entryNodes printers
            | FILES <- entryType = []
            | Format.LONG <- Config.format config = Formatter.mkBlockSizeHeader entryNodes printers
            | otherwise = []
       in filter (not . null) ([header] <> [totalBlockSize]) <> (mconcat <$> Grid.mkValidGrid config (Config.width config) nodes)
    PrintTree (Tree {..}) -> Formatter.mkLines treeNodes config printers $ Formatter.mkPrinterTypes config
