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
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Functor
import Data.List (foldl', intersperse, partition)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Haskellorls.Class
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Depth as Depth
import Haskellorls.Config.Listing
import Haskellorls.Control.Exception.Safe
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

newtype LsConf = LsConf (Config.Config, Formatter.Printers)

newtype LsState = LsState ([Operation], Walk.AlreadySeenInodes, Dired.NameIndeces, [SomeException])

-- | A central piece monad of haskellorls.
--
-- NOTE: This is inspired from XMonad.
-- <https://github.com/xmonad/xmonad/blob/a902fefaf1f27f1a21dc35ece15e7dbb573f3d95/src/XMonad/Core.hs#L158>
newtype Ls a = Ls (ReaderT LsConf (StateT LsState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadReader LsConf, MonadState LsState)

runLs :: LsConf -> LsState -> Ls a -> IO (a, LsState)
runLs c s (Ls a) = runStateT (runReaderT a c) s

run :: LsConf -> LsState -> IO ((), LsState)
run conf st = runLs conf st go
  where
    go =
      get >>= \case
        LsState ([], _, indeces, _) -> do
          c <- asks $ \(LsConf (c, _)) -> c

          when (Config.dired c) $ do
            unless (null $ Dired.dired indeces) $
              liftIO . T.putStrLn . TL.toStrict . TL.toLazyText $
                "//DIRED//" <> foldl' (\acc (x, y) -> foldr (\t acc' -> TL.fromText t <> acc') acc [" ", T.pack (show x), " ", T.pack (show y)]) mempty (Dired.dired indeces)

            unless (null $ Dired.subdired indeces) $
              liftIO . T.putStrLn . TL.toStrict . TL.toLazyText $
                "//SUBDIRED//" <> foldl' (\acc (x, y) -> foldr (\t acc' -> TL.fromText t <> acc') acc [" ", T.pack (show x), " ", T.pack (show y)]) mempty (Dired.subdired indeces)

            liftIO . T.putStrLn $
              "//DIRED-OPTIONS// --quoting-style="
                <> case Config.quotingStyle c of
                  Quote.NoStyle -> "shell-escape"
                  Quote.Literal -> "literal"
                  Quote.Shell -> "shell"
                  Quote.ShellAlways -> "shell-always"
                  Quote.ShellEscape -> "shell-escape"
                  Quote.ShellEscapeAlways -> "shell-escape-always"
                  Quote.C -> "c"
                  Quote.Escape -> "escape"
        LsState (op : ops, inodes, indeces, errors) -> do
          c@(LsConf (config, printers)) <- ask

          (op', errs) <- case op of
            (PrintTree (Tree {..})) -> do
              -- FIXME: Move directory traversing into an appropriate place.
              (nodes, errs) <- Tree.mkTreeNodeInfos config treeNode
              return (PrintTree $ Tree {treeNodes = nodes, ..}, errs)
            _ -> return (op, mempty)

          let divs = mkDivisions config printers op'
              indeces' =
                if Config.dired config
                  then foldl' (\acc wt -> Dired.update (T.encodeUtf8 . WT.wtWord <$> wt) acc) indeces divs
                  else indeces

          liftIO . T.putStr . TL.toStrict . TL.toLazyText $ foldr (\x acc -> (TL.fromText . serialize $ Attr.unwrap x) <> acc) mempty divs

          put =<< updateLsState c (LsState (op' : ops, inodes, indeces', errs <> errors))

          go

updateLsState :: (MonadCatch m, MonadIO m) => LsConf -> LsState -> m LsState
updateLsState _ s@(LsState ([], _, _, _)) = pure s
updateLsState c@(LsConf (config, _)) (LsState (op : ops, inodes, indeces, errors)) = case op of
  PrintEntry (Entry {..})
    | Config.recursive config && entryDepth < Config.level config -> do
        let (nodes, newInodes) = runState (Walk.updateAlreadySeenInode $ filter (Node.isDirectory . Node.nodeType) entryNodes) inodes
            paths = map (\node -> entryPath </> Node.getNodePath node) nodes
        (errs, ops') <- partitionEithers <$> mapM (tryIO . pathToOp c (Depth.increaseDepth entryDepth)) paths
        mapM_ printErr errs
        let entries = (\es -> if null es then es else Newline : es) $ intersperse Newline ops'
        pure $ LsState (entries <> ops, newInodes, indeces, (toException <$> errs) <> errors)
  _ -> pure $ LsState (ops, inodes, indeces, errors)

pathToOp :: (MonadCatch m, MonadIO m) => LsConf -> Depth.Depth -> RawFilePath -> m Operation
pathToOp (LsConf (config, _)) depth path = do
  nodes <- buildDirectoryNodes config path
  pure . PrintEntry $ Entry DIRECTORY path nodes config depth

-- | With error message output.
buildDirectoryNodes :: (MonadCatch m, MonadIO m) => Config.Config -> RawFilePath -> m [Node.NodeInfo]
buildDirectoryNodes config path = do
  contents <- Listing.listContents config path
  Sort.sort config <$> (mapM (Node.mkNodeInfo config path) . excluder) contents
  where
    excluder = ignoreExcluder . hideExcluder
    ignorePtn = Config.ignore config
    hidePtn = Config.hide config
    isShowHiddenEntries = case Config.listing config of
      NoHidden -> False
      _ -> True
    ignoreExcluder
      | null ignorePtn = id
      | otherwise = Listing.exclude ignorePtn
    hideExcluder
      | null hidePtn || isShowHiddenEntries = id
      | otherwise = Listing.exclude hidePtn

mkInitialOperations :: (MonadCatch m, MonadIO m) => LsConf -> [RawFilePath] -> m ([Operation], Walk.AlreadySeenInodes, [SomeException])
mkInitialOperations c@(LsConf (config@Config.Config {tree}, _)) paths = do
  (errs, nodeinfos) <- first (map toException) . partitionEithers <$> mapM (tryIO . Node.mkNodeInfo config "") paths

  mapM_ printErr errs

  let (nodes, inodes) = runState (Walk.updateAlreadySeenInode $ Sort.sort config nodeinfos) mempty
  let (dirs, files) = partition isDirectory nodes
      fileOp = [PrintEntry (Entry FILES "" files config depth) | not (null files)]

  if tree
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
    depth = Depth.increaseDepth Depth.makeZero
    isDirectory
      | Config.directory config = const False
      | otherwise = Node.isDirectory . Node.nodeType

mkDivisions :: Config.Config -> Formatter.Printers -> Operation -> [Attr.Attribute WT.WrappedText]
mkDivisions config printers =
  foldr (\x acc -> [Attr.Other $ deserialize "  " | Config.dired config && not (null x)] <> x <> [Attr.Other . deserialize $ if Config.zero config then "\0" else "\n"] <> acc) mempty . \case
    Newline -> [[]]
    PrintEntry (Entry {..}) -> filter (not . null) ([header] <> [totalBlockSize]) <> (mconcat <$> Grid.buildValidGrid config (Config.width config) nodes')
      where
        config' =
          if shouldQuote entryNodes
            then config
            else config {Config.noQuote = True}

        nodes' = Formatter.buildLines entryNodes printers $ Formatter.buildPrinterTypes config'

        header = case entryType of
          FILES -> []
          SINGLEDIR | not (Config.recursive config) -> []
          _ -> [Attr.Dir . deserialize $ T.decodeUtf8 entryPath, Attr.Other $ deserialize ":"]

        -- Add total block size header only about directries when long style
        -- layout or @-s / --size@ is passed.
        totalBlockSize = case entryType of
          _ | Config.size config -> [size]
          FILES -> []
          _
            | Config.isLongStyle config -> [size]
            | otherwise -> []
          where
            size = Attr.Other . deserialize . T.concat . ("total " :) . map (WT.serialize . Attr.unwrap) . Size.toTotalBlockSize config $ Node.fileSize <$> entryNodes
    PrintTree (Tree {..}) ->
      Formatter.buildLines treeNodes printers . Formatter.buildPrinterTypes $
        if shouldQuote treeNodes
          then config
          else config {Config.noQuote = True}

shouldQuote :: Foldable t => t Node.NodeInfo -> Bool
shouldQuote = not . all (S.null . S.intersection setNeedQuotes . C.foldr S.insert mempty . Node.getNodePath)
  where
    setNeedQuotes = S.fromList Quote.charactorsNeedQuote
