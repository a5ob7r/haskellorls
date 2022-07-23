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
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Listing
import qualified Haskellorls.Depth as Depth
import Haskellorls.Exception
import qualified Haskellorls.Formatter as Formatter
import qualified Haskellorls.Formatter.Layout.Grid as Grid
import qualified Haskellorls.Formatter.Quote as Quote
import qualified Haskellorls.Formatter.Size as Size
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Recursive.Tree as Tree
import qualified Haskellorls.Recursive.Utils as Recursive
import qualified Haskellorls.Sort as Sort
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import System.FilePath.Posix.ByteString

data EntryType = FILES | SINGLEDIR | DIRECTORY

data Entry = Entry
  { entryType :: EntryType,
    entryPath :: RawFilePath,
    entryNodes :: [Node.NodeInfo],
    entryConfig :: Config.Config,
    entryDepth :: Depth.Depth
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

newtype Printer = Printer (Operation -> T.Text)

newtype LsConf = LsConf (Config.Config, Printer)

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
          c'@(LsConf (config, Printer printer)) <- ask

          errs <- liftIO $ do
            (op', errs) <- case op of
              (PrintTree (Tree {..})) -> do
                -- TODO: Move directory traversing into an appropriate place.
                (nodes, errs) <- Tree.mkTreeNodeInfos config treeNode
                return (PrintTree $ Tree {treeNodes = nodes, ..}, errs)
              _ -> return (op, mempty)

            T.putStrLn $ printer op'

            return errs

          modify (\(LsState (ops, inodes, errors)) -> LsState (ops, inodes, errs <> errors))

          put =<< liftIO (newLsState c' s')

          go

newLsState :: LsConf -> LsState -> IO LsState
newLsState _ s@(LsState ([], _, _)) = pure s
newLsState c@(LsConf (config, _)) (LsState (op : ops, inodes, errors)) = case op of
  PrintEntry (Entry {..})
    | Config.recursive config && entryDepth < Config.level config -> do
        let (nodes, newInodes) = runState (Recursive.updateAlreadySeenInode $ filter (Node.isDirectory . Node.nodeType) entryNodes) inodes
            paths = map (\node -> entryPath </> Node.getNodePath node) nodes
        (errs, ops') <- partitionEithers <$> mapM (tryIO . pathToOp c (Depth.increaseDepth entryDepth)) paths
        mapM_ printErr errs
        let entries = (\es -> if null es then es else Newline : es) $ L.intersperse Newline ops'
        pure $ LsState (entries <> ops, newInodes, (toException <$> errs) <> errors)
  _ -> pure $ LsState (ops, inodes, errors)

pathToOp :: (MonadCatch m, MonadIO m) => LsConf -> Depth.Depth -> RawFilePath -> m Operation
pathToOp (LsConf (config, _)) depth path = do
  nodes <- buildDirectoryNodes config path
  pure . PrintEntry $ Entry DIRECTORY path nodes config depth

-- | With error message output.
buildDirectoryNodes :: (MonadCatch m, MonadIO m) => Config.Config -> RawFilePath -> m [Node.NodeInfo]
buildDirectoryNodes config path = do
  contents <- Utils.listContents config path
  Sort.sorter config <$> (mapM (Node.mkNodeInfo config path) . excluder) contents
  where
    excluder = ignoreExcluder . hideExcluder
    ignorePtn = Config.ignore config
    hidePtn = Config.hide config
    isShowHiddenEntries = case Config.listing config of
      NoHidden -> False
      _ -> True
    ignoreExcluder
      | null ignorePtn = id
      | otherwise = Utils.exclude ignorePtn
    hideExcluder
      | null hidePtn || isShowHiddenEntries = id
      | otherwise = Utils.exclude hidePtn

mkInitialOperations :: (MonadCatch m, MonadIO m) => LsConf -> [RawFilePath] -> m ([Operation], Recursive.AlreadySeenInodes, [SomeException])
mkInitialOperations c@(LsConf (config@Config.Config {tree}, _)) paths = do
  (errs, nodeinfos) <- first (map toException) . partitionEithers <$> mapM (tryIO . Node.mkNodeInfo config "") paths

  mapM_ (liftIO . printErr) errs

  let (nodes, inodes) = runState (Recursive.updateAlreadySeenInode $ Sort.sorter config nodeinfos) mempty
  let (dirs, files) = L.partition isDirectory nodes
      fileOp = [PrintEntry (Entry FILES "" files config depth) | not (null files)]

  if tree
    then do
      let ops = dirs <&> \node -> PrintTree $ Tree (Node.getNodePath node) node mempty config
      return (L.intersperse Newline $ fileOp <> ops, inodes, errs)
    else do
      dirOps <-
        mapM (pathToOp c depth . Node.getNodePath) dirs <&> \case
          -- Considers no argument to be also a single directory.
          [PrintEntry e] | null files && length paths < 2 -> [PrintEntry (e {entryType = SINGLEDIR})]
          ops -> ops
      return (L.intersperse Newline $ fileOp <> dirOps, inodes, errs)
  where
    depth = Depth.increaseDepth Depth.makeZero
    isDirectory
      | Config.directory config = const False
      | otherwise = Node.isDirectory . Node.nodeType

buildPrinter :: Config.Config -> Formatter.Printers -> Printer
buildPrinter config printers = Printer $ (TL.toStrict . TL.toLazyText . mconcat . L.intersperse (TL.fromText "\n")) . generateEntryLines config printers

generateEntryLines :: Config.Config -> Formatter.Printers -> Operation -> [TL.Builder]
generateEntryLines config printers op = case op of
  Newline -> []
  PrintEntry (Entry {..}) ->
    addHeader . addTotalBlockSize . Grid.renderGrid $ Grid.buildValidGrid config (Config.width config) nodes'
    where
      config' =
        if shouldQuote entryNodes
          then config
          else config {Config.noQuote = True}
      nodes' = Formatter.buildLines entryNodes printers $ Formatter.buildPrinterTypes config'
      addHeader = case entryType of
        FILES -> id
        SINGLEDIR | not (Config.recursive config) -> id
        _ -> \ss -> TL.fromText (T.decodeUtf8 entryPath `T.snoc` ':') : ss

      -- Add total block size header only about directries when long style
      -- layout or `-s / --size` is passed.
      addTotalBlockSize = case entryType of
        _ | Config.size config -> (builder :)
        FILES -> id
        _
          | Config.isLongStyle config -> (builder :)
          | otherwise -> id
        where
          builder = TL.fromText . T.concat . ("total " :) . map WT.serialize . Size.toTotalBlockSize config $ map Node.fileSize entryNodes
  PrintTree (Tree {..}) ->
    map (foldMap $ TL.fromText . WT.serialize) . Formatter.buildLines treeNodes printers $ Formatter.buildPrinterTypes config'
    where
      config' =
        if shouldQuote treeNodes
          then config
          else config {Config.noQuote = True}

shouldQuote :: Foldable t => t Node.NodeInfo -> Bool
shouldQuote = not . all (S.null . S.intersection setNeedQuotes . C.foldr S.insert mempty . Node.getNodePath)
  where
    setNeedQuotes = S.fromList Quote.charactorsNeedQuote
