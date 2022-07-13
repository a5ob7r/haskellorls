module Haskellorls.Recursive
  ( buildPrinter,
    mkInitialOperations,
    run,
    LsConf (..),
    LsState (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Functor
import qualified Data.List as L
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
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
import System.FilePath.Posix.ByteString

data EntryType = FILES | SINGLEDIR | DIRECTORY

data Entry = Entry
  { entryType :: EntryType,
    entryPath :: RawFilePath,
    entryNodes :: [Node.NodeInfo],
    entryOption :: Option.Option,
    entryDepth :: Depth.Depth
  }

data Tree = Tree
  { treePath :: RawFilePath,
    treeNode :: Node.NodeInfo,
    treeNodes :: Seq.Seq Node.NodeInfo,
    treeOption :: Option.Option
  }

data Operation
  = Newline
  | PrintEntry Entry
  | PrintTree Tree

newtype Printer = Printer (Operation -> T.Text)

newtype LsConf = LsConf (Option.Option, Printer)

newtype LsState = LsState ([Operation], Recursive.AlreadySeenInodes, [SomeException])

-- | A central piece monad of haskellorls.
--
-- NOTE: This is inspired from XMonad.
-- <https://github.com/xmonad/xmonad/blob/a902fefaf1f27f1a21dc35ece15e7dbb573f3d95/src/XMonad/Core.hs#L158>
newtype Ls a = Ls (ReaderT LsConf (StateT LsState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LsConf, MonadState LsState)

runLs :: LsConf -> LsState -> Ls a -> IO (a, LsState)
runLs c s (Ls a) = runStateT (runReaderT a c) s

run :: LsConf -> LsState -> IO ((), LsState)
run c s = runLs c s go
  where
    go =
      get >>= \case
        LsState ([], _, _) -> pure ()
        s'@(LsState (op : _, _, _)) -> do
          c'@(LsConf (opt, Printer printer)) <- ask

          errs <- liftIO $ do
            (op', errs) <- case op of
              (PrintTree (Tree {..})) -> do
                -- TODO: Move directory traversing into an appropriate place.
                (nodes, errs) <- Tree.mkTreeNodeInfos opt treeNode
                return (PrintTree $ Tree {treeNodes = nodes, ..}, errs)
              _ -> return (op, mempty)

            T.putStrLn $ printer op'

            return errs

          modify (\(LsState (ops, inodes, errors)) -> LsState (ops, inodes, errs <> errors))

          put =<< liftIO (newLsState c' s')

          go

newLsState :: LsConf -> LsState -> IO LsState
newLsState _ s@(LsState ([], _, _)) = pure s
newLsState c@(LsConf (opt, _)) (LsState (op : ops, inodes, errors)) = case op of
  PrintEntry (Entry {..})
    | Option.recursive opt && entryDepth < Option.level opt -> do
        let (nodes, newInodes) = runState (Recursive.updateAlreadySeenInode $ filter (Node.isDirectory . Node.nodeType) entryNodes) inodes
            paths = map (\node -> entryPath </> Node.getNodePath node) nodes
        (errs, ops') <- partitionEithers <$> mapM (tryIO . pathToOp c (Depth.increaseDepth entryDepth)) paths
        mapM_ printErr errs
        let entries = (\es -> if null es then es else Newline : es) $ L.intersperse Newline ops'
        pure $ LsState (entries <> ops, newInodes, (toException <$> errs) <> errors)
  _ -> pure $ LsState (ops, inodes, errors)

pathToOp :: (MonadCatch m, MonadIO m) => LsConf -> Depth.Depth -> RawFilePath -> m Operation
pathToOp (LsConf (opt, _)) depth path = do
  nodes <- buildDirectoryNodes opt path
  pure . PrintEntry $ Entry DIRECTORY path nodes opt depth

-- | With error message output.
buildDirectoryNodes :: (MonadCatch m, MonadIO m) => Option.Option -> RawFilePath -> m [Node.NodeInfo]
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

mkInitialOperations :: (MonadCatch m, MonadIO m) => LsConf -> [RawFilePath] -> m ([Operation], Recursive.AlreadySeenInodes, [SomeException])
mkInitialOperations c@(LsConf (opt@Option.Option {tree}, _)) paths = do
  (errs, nodeinfos) <- first (map toException) . partitionEithers <$> mapM (tryIO . Node.mkNodeInfo opt "") paths

  mapM_ (liftIO . printErr) errs

  let (nodes, inodes) = runState (Recursive.updateAlreadySeenInode $ Sort.sorter opt nodeinfos) mempty
  let (dirs, files) = L.partition isDirectory nodes
      fileOp = [PrintEntry (Entry FILES "" files opt depth) | not (null files)]

  if tree
    then do
      let ops = dirs <&> \node -> PrintTree $ Tree (Node.getNodePath node) node mempty opt
      return (L.intersperse Newline $ fileOp <> ops, inodes, errs)
    else do
      dirOps <-
        mapM (pathToOp c depth . Node.getNodePath) dirs <&> \case
          -- Considers no argument to be also a single directory.
          [PrintEntry e] | null files && length (Option.targets opt) < 2 -> [PrintEntry (e {entryType = SINGLEDIR})]
          ops -> ops
      return (L.intersperse Newline $ fileOp <> dirOps, inodes, errs)
  where
    depth = Depth.increaseDepth Depth.makeZero
    isDirectory
      | Option.directory opt = const False
      | otherwise = Node.isDirectory . Node.nodeType

buildPrinter :: Option.Option -> Decorator.Printers -> Printer
buildPrinter opt printers = Printer $ (TL.toStrict . TL.toLazyText . mconcat . L.intersperse (TL.fromText "\n")) . generateEntryLines opt printers

generateEntryLines :: Option.Option -> Decorator.Printers -> Operation -> [TL.Builder]
generateEntryLines opt printers op = case op of
  Newline -> []
  PrintEntry (Entry {..}) ->
    addHeader . addTotalBlockSize . Grid.renderGrid $ Grid.buildValidGrid opt (Option.columnSize opt) nodes'
    where
      opt' =
        if shouldQuote entryNodes
          then opt
          else opt {Option.noQuote = True}
      nodes' = Decorator.buildLines entryNodes printers $ Decorator.buildPrinterTypes opt'
      addHeader = case entryType of
        FILES -> id
        SINGLEDIR | not (Option.recursive opt) -> id
        _ -> \ss -> TL.fromText (T.decodeUtf8 entryPath `T.snoc` ':') : ss

      -- Add total block size header only about directries when long style
      -- layout or `-s / --size` is passed.
      addTotalBlockSize = case entryType of
        _ | Option.size opt -> (builder :)
        FILES -> id
        _
          | Format.isLongStyle opt -> (builder :)
          | otherwise -> id
        where
          builder = TL.fromText . T.concat . ("total " :) . map WT.serialize . Size.toTotalBlockSize opt $ map Node.fileSize entryNodes
  PrintTree (Tree {..}) ->
    map (foldMap $ TL.fromText . WT.serialize) . Decorator.buildLines treeNodes printers $ Decorator.buildPrinterTypes opt'
    where
      opt' =
        if shouldQuote treeNodes
          then opt
          else opt {Option.noQuote = True}

shouldQuote :: Foldable t => t Node.NodeInfo -> Bool
shouldQuote = not . all (S.null . S.intersection setNeedQuotes . C.foldr S.insert mempty . Node.getNodePath)
  where
    setNeedQuotes = S.fromList Quote.charactorsNeedQuote
