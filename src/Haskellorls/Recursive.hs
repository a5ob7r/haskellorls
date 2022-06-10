module Haskellorls.Recursive
  ( buildPrinter,
    buildInitialOperations,
    generateEntryLines,
    run,
    LsConf (..),
    LsState (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Either as E
import qualified Data.Foldable as Fold
import Data.Functor
import qualified Data.List as L
import qualified Data.Monoid as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Depth as Depth
import Haskellorls.Exception
import qualified Haskellorls.Format.Grid as Grid
import qualified Haskellorls.Format.Util as Format
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Recursive.Utils as Recursive
import qualified Haskellorls.Size.Decorator as Size
import qualified Haskellorls.Sort.Method as Sort
import qualified Haskellorls.Tree.Util as Tree
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import qualified System.FilePath.Posix as Posix

data EntryType = FILES | SINGLEDIR | DIRECTORY

data Entry = Entry
  { entryType :: EntryType,
    entryPath :: FilePath,
    entryNodes :: [Node.NodeInfo],
    entryOption :: Option.Option,
    entryDepth :: Depth.Depth
  }

data Tree = Tree
  { treePath :: FilePath,
    treeOption :: Option.Option
  }

data Operation
  = Newline
  | PrintEntry Entry
  | PrintTree Tree

newtype Printer = Printer (Operation -> IO T.Text)

newtype LsConf = LsConf (Option.Option, Printer)

newtype LsState = LsState ([Operation], Recursive.AlreadySeenInodes)

-- | A central piece monad of haskellorls.
--
-- NOTE: This is inspired from XMonad.
-- https://github.com/xmonad/xmonad/blob/a902fefaf1f27f1a21dc35ece15e7dbb573f3d95/src/XMonad/Core.hs#L158
newtype RuntimeContext a = RuntimeContext (ReaderT LsConf (StateT LsState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LsConf, MonadState LsState)

type Ls = RuntimeContext ()

runLs :: LsConf -> LsState -> Ls -> IO ((), LsState)
runLs c s (RuntimeContext a) = runStateT (runReaderT a c) s

run :: LsConf -> LsState -> IO ((), LsState)
run c s = runLs c s go
  where
    go =
      get >>= \case
        LsState ([], _) -> pure ()
        s'@(LsState (op : _, _)) -> do
          c'@(LsConf (_, Printer printer)) <- ask

          liftIO $ T.putStrLn =<< printer op
          put =<< liftIO (newLsState c' s')

          go

newLsState :: LsConf -> LsState -> IO LsState
newLsState _ s@(LsState ([], _)) = pure s
newLsState c@(LsConf (opt, _)) (LsState (op : ops, inodes)) = case op of
  PrintEntry (Entry {..})
    | Option.recursive opt && entryDepth < Option.level opt -> do
        let (nodes, newInodes) = runState (Recursive.updateAlreadySeenInode $ filter (Node.isDirectory . Node.nodeType) entryNodes) inodes
            paths = map (\node -> entryPath Posix.</> Node.getNodePath node) nodes
        (errs, ops') <- E.partitionEithers <$> mapM (tryIO . pathToOp c (Depth.increaseDepth entryDepth)) paths
        mapM_ printErr errs
        let entries = (\es -> if null es then es else Newline : es) $ L.intersperse Newline ops'
        pure $ LsState (entries <> ops, newInodes)
  _ -> pure $ LsState (ops, inodes)

pathToOp :: (MonadCatch m, MonadIO m) => LsConf -> Depth.Depth -> FilePath -> m Operation
pathToOp (LsConf (opt, _)) depth path = do
  nodes <- buildDirectoryNodes opt path
  pure . PrintEntry $ Entry DIRECTORY path nodes opt depth

-- | With error message output.
buildDirectoryNodes :: (MonadCatch m, MonadIO m) => Option.Option -> FilePath -> m [Node.NodeInfo]
buildDirectoryNodes opt path = do
  contents <- Utils.listContents opt path
  Sort.sorter opt <$> (mapM (Node.mkNodeInfo opt path) . excluder) contents
  where
    excluder = ignoreExcluder . hideExcluder
    ignorePtn = Option.ignore opt
    hidePtn = Option.hide opt
    isShowHiddenEntries = Option.all opt || Option.almostAll opt
    ignoreExcluder
      | null ignorePtn = id
      | otherwise = Utils.exclude ignorePtn
    hideExcluder
      | null hidePtn || isShowHiddenEntries = id
      | otherwise = Utils.exclude hidePtn

-- | Assumes all paths exist.
buildInitialOperations :: (MonadCatch m, MonadIO m) => LsConf -> [FilePath] -> m (Recursive.AlreadySeenInodes, [Operation])
buildInitialOperations c@(LsConf (opt, _)) paths = do
  (errs, nodeinfos) <- E.partitionEithers <$> mapM (tryIO . Node.mkNodeInfo opt "") paths

  mapM_ (liftIO . printErr) errs

  let (nodes, inodes) = runState (Recursive.updateAlreadySeenInode $ Sort.sorter opt nodeinfos) Recursive.def
  let (dirs, files) = L.partition isDirectory nodes
      fileOp = [PrintEntry (Entry FILES "" files opt depth) | not (null files)]
  dirOps <- mapM (pathToOp c depth . Node.getNodePath) dirs
  let dirOps' =
        if Option.tree opt
          then
            dirOps <&> \case
              (PrintEntry (Entry {..})) -> PrintTree (Tree entryPath entryOption)
              o -> o
          else case dirOps of
            -- Considers no argument to be also a single directory.
            [PrintEntry e] | null files && length (Option.targets opt) < 2 -> [PrintEntry (e {entryType = SINGLEDIR})]
            _ -> dirOps
  pure (inodes, L.intersperse Newline $ fileOp <> dirOps')
  where
    depth = Depth.increaseDepth Depth.makeZero
    isDirectory
      | Option.directory opt = const False
      | otherwise = Node.isDirectory . Node.nodeType

buildPrinter :: Option.Option -> Decorator.Printers -> Printer
buildPrinter opt printers = Printer $ fmap (TL.toStrict . TLB.toLazyText . M.mconcat . L.intersperse (TLB.fromText "\n")) . generateEntryLines opt printers

generateEntryLines :: Option.Option -> Decorator.Printers -> Operation -> IO [TLB.Builder]
generateEntryLines opt printers op = case op of
  Newline -> pure []
  PrintEntry (Entry {..}) -> do
    let opt' = if shouldQuote entryNodes then opt else opt {Option.noQuote = True}
        nodes' = Decorator.buildLines entryNodes printers $ Decorator.buildPrinterTypes opt'
        addHeader = case entryType of
          FILES -> id
          SINGLEDIR | not (Option.recursive opt) -> id
          _ -> \ss -> TLB.fromText (T.pack entryPath `T.snoc` ':') : ss

        -- Add total block size header only about directries when long style
        -- layout or `-s / --size` is passed.
        addTotalBlockSize = case entryType of
          _ | Option.size opt -> (builder :)
          FILES -> id
          _
            | Format.isLongStyle opt -> (builder :)
            | otherwise -> id
          where
            builder = TLB.fromText . T.concat . ("total " :) . map WT.serialize . Size.toTotalBlockSize opt $ map Node.fileSize entryNodes

    colLen <- Grid.virtualColumnSize opt

    return . addHeader . addTotalBlockSize . Grid.renderGrid $ Grid.buildValidGrid opt colLen nodes'
  PrintTree (Tree {..}) -> do
    nodes <- Fold.toList <$> Tree.makeTreeNodeInfos opt treePath
    let opt' = if shouldQuote nodes then opt else opt {Option.noQuote = True}
    pure . map (M.mconcat . map wtToBuilder) $ Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt'
    where
      wtToBuilder = TLB.fromText . WT.serialize

shouldQuote :: [Node.NodeInfo] -> Bool
shouldQuote = not . all (Set.null . Set.intersection setNeedQuotes . Set.fromList . Node.getNodePath)
  where
    setNeedQuotes = Set.fromList Quote.charactorsNeedQuote
