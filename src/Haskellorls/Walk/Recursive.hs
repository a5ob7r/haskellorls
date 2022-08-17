module Haskellorls.Walk.Recursive
  ( mkInitialOperations,
    run,
    LsConf (..),
    LsState (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Either
import Data.Functor
import Data.List (foldl', intersperse, partition)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Haskellorls.Class
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Format as Format
import Haskellorls.Control.Exception.Safe
import Haskellorls.Data.Infinitable
import qualified Haskellorls.Formatter as Formatter
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.Layout.Grid as Grid
import qualified Haskellorls.Formatter.Quote as Quote
import qualified Haskellorls.Formatter.Size as Size
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Walk.Dired as Dired
import qualified Haskellorls.Walk.Listing as Listing
import qualified Haskellorls.Walk.Sort as Sort
import qualified Haskellorls.Walk.Tree as Tree
import qualified Haskellorls.Walk.Utils as Walk
import System.FilePath.Posix.ByteString

data EntryType = FILES | SINGLEDIR | DIRECTORY
  deriving (Eq)

data Entry = Entry
  { entryType :: EntryType,
    entryPath :: RawFilePath,
    entryNodes :: [Node.NodeInfo],
    entryConfig :: Config.Config,
    entryDepth :: Infinitable Int
  }

data Tree = Tree
  { treePath :: RawFilePath,
    treeNode :: Node.NodeInfo,
    treeNodes :: Seq.Seq Node.NodeInfo,
    treeConfig :: Config.Config
  }

data Operation
  = Newline
  | PrintEntry Entry
  | PrintTree Tree

data LsConf = LsConf Config.Config Formatter.Printers

data LsState = LsState
  { inodes :: Walk.Inodes,
    indices :: Dired.NameIndeces,
    errors :: [SomeException]
  }

-- | A central piece monad of haskellorls.
--
-- NOTE: This is inspired from XMonad.
-- <https://github.com/xmonad/xmonad/blob/a902fefaf1f27f1a21dc35ece15e7dbb573f3d95/src/XMonad/Core.hs#L158>
newtype Ls a = Ls (ReaderT LsConf (StateT LsState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader LsConf, MonadState LsState)

runLs :: LsConf -> LsState -> Ls a -> IO (a, LsState)
runLs conf st (Ls a) = runStateT (runReaderT a conf) st

run :: LsConf -> LsState -> [Operation] -> IO ([Operation], LsState)
run conf st operations = runLs conf st $ go operations
  where
    go [] = do
      LsConf config _ <- ask

      when (Config.dired config) $ do
        indeces <- gets indices

        unless (null $ Dired.dired indeces) $
          liftIO . T.putStrLn . TL.toStrict . TL.toLazyText $
            "//DIRED//" <> foldl' (\acc (x, y) -> foldr (\t acc' -> TL.fromText t <> acc') acc [" ", T.pack (show x), " ", T.pack (show y)]) mempty (Dired.dired indeces)

        unless (null $ Dired.subdired indeces) $
          liftIO . T.putStrLn . TL.toStrict . TL.toLazyText $
            "//SUBDIRED//" <> foldl' (\acc (x, y) -> foldr (\t acc' -> TL.fromText t <> acc') acc [" ", T.pack (show x), " ", T.pack (show y)]) mempty (Dired.subdired indeces)

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

      return []
    go (op : ops) = do
      c@(LsConf config printers) <- ask

      op' <- case op of
        (PrintTree (Tree {..})) -> do
          -- FIXME: Move directory traversing into an appropriate place.
          (nodes, errs) <- Tree.mkTreeNodeInfos config treeNode

          modify $ \s -> s {errors = errs <> errors s}

          return . PrintTree $ Tree {treeNodes = nodes, ..}
        _ -> return op

      let f acc d = do
            liftIO . T.putStr . from $ Attr.unwrap d

            return $!
              if Config.dired config
                then Dired.update (T.encodeUtf8 . WT.wtWord <$> d) acc
                else acc
          divs = mkDivisions config printers op'
       in gets indices >>= \x -> foldM f x divs >>= \x' -> modify $ \s -> s {indices = x'}

      case op' of
        PrintEntry (Entry {..}) | Config.recursive config && entryDepth < Config.level config -> do
          (nodes, newInodes) <- gets $ runState (Walk.filterNodes $ filter (Node.isDirectory . Node.nodeType) entryNodes) . inodes
          modify $ \s -> s {inodes = newInodes}

          let paths = (\node -> entryPath </> Node.getNodePath node) <$> nodes

          (errs, ops') <- partitionEithers <$> mapM (tryIO . pathToOp c (succ <$> entryDepth)) paths
          mapM_ printErr errs
          modify $ \s -> s {errors = (toException <$> errs) <> errors s}

          let entries = (\es -> if null es then es else Newline : es) $ intersperse Newline ops'

          go $ entries <> ops
        _ -> go ops

pathToOp :: (MonadCatch m, MonadIO m) => LsConf -> Infinitable Int -> RawFilePath -> m Operation
pathToOp (LsConf config _) depth path = do
  nodes <- Sort.sort config <$> (mapM (Node.mkNodeInfo config path) =<< Listing.listContents config path)
  return . PrintEntry $ Entry DIRECTORY path nodes config depth

mkInitialOperations :: (MonadCatch m, MonadIO m) => LsConf -> [RawFilePath] -> m ([Operation], Walk.Inodes, [SomeException])
mkInitialOperations c@(LsConf config _) paths = do
  (errs, nodeinfos) <- first (map toException) . partitionEithers <$> mapM (tryIO . Node.mkNodeInfo config "") paths

  mapM_ printErr errs

  let (nodes, inodes) = runState (Walk.filterNodes $ Sort.sort config nodeinfos) mempty
  let (dirs, files) = partition isDirectory nodes
      fileOp = [PrintEntry (Entry FILES "" files config depth) | not (null files)]

  if Config.tree config
    then do
      let ops = dirs <&> \node -> PrintTree $ Tree (Node.getNodePath node) node mempty config
      return (intersperse Newline $ fileOp <> ops, inodes, errs)
    else do
      dirOps <-
        mapM (pathToOp c depth . Node.getNodePath) dirs <&> \case
          -- Considers no argument to be also a single directory.
          [PrintEntry e] | null files && length paths < 2 -> [PrintEntry (e {entryType = SINGLEDIR})]
          ops -> ops
      return (intersperse Newline $ fileOp <> dirOps, inodes, errs)
  where
    depth = Only 1
    isDirectory
      | Config.directory config = const False
      | otherwise = Node.isDirectory . Node.nodeType

mkDivisions :: Config.Config -> Formatter.Printers -> Operation -> [Attr.Attribute WT.WrappedText]
mkDivisions config printers =
  foldr (\x acc -> [Attr.Other $ from @T.Text "  " | Config.dired config && not (null x)] <> x <> [Attr.Other . from @T.Text $ if Config.zero config then "\0" else "\n"] <> acc) mempty . \case
    Newline -> [[]]
    PrintEntry (Entry {..}) ->
      let nodes = Formatter.mkLines entryNodes config printers $ Formatter.mkPrinterTypes config

          header = case entryType of
            FILES -> []
            SINGLEDIR | not (Config.recursive config) -> []
            _ -> [Attr.Dir . from $ T.decodeUtf8 entryPath, Attr.Other $ from @T.Text ":"]

          -- Add total block size header only about directries when long style
          -- layout or @-s / --size@ is passed.
          size = Attr.Other . from . T.concat . ("total " :) . map (from . Attr.unwrap) . Size.toTotalBlockSize config $ Node.fileSize <$> entryNodes
          totalBlockSize
            | Config.size config = [size]
            | FILES <- entryType = []
            | Format.LONG <- Config.format config = [size]
            | otherwise = []
       in filter (not . null) ([header] <> [totalBlockSize]) <> (mconcat <$> Grid.mkValidGrid config (Config.width config) nodes)
    PrintTree (Tree {..}) -> Formatter.mkLines treeNodes config printers $ Formatter.mkPrinterTypes config
