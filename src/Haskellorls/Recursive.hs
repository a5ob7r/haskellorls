{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Haskellorls.Recursive
  ( buildPrinter,
    buildInitialOperations,
    exec,
    generateEntryLines,
  )
where

import qualified Control.Monad.State.Strict as State
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
import qualified System.IO as IO

data EntryType = FILES | SINGLEDIR | DIRECTORY

data Operation
  = Newline
  | PrintEntry
      { entryType :: EntryType,
        entryPath :: FilePath,
        entryNodes :: [Node.NodeInfo],
        entryOption :: Option.Option
      }
  | PrintTree
      { entryPath :: FilePath,
        entryOption :: Option.Option
      }

newtype Printer = Printer (Operation -> IO T.Text)

-- | NOTE: Execute output operation with stack data structure. This way may
-- cause performance down about total execution time. But user can see output
-- to stdout immediately. It leads to good user experiences. This focuses large
-- output situation like --recursive/-R option.
--
-- Another way is concatenating many text using builder aa long as possible. It
-- reduces text building cost maximally. But can not output until get all text
-- and concatenate them. So it causes output delay. This is bad user
-- experiences.
exec :: Recursive.AlreadySeenInodes -> Printer -> [Operation] -> IO ()
exec _ _ [] = pure ()
exec inodes printer (op : stack) = do
  (newInodes, ops) <- eval inodes printer op
  let entries = (\es -> if null es then es else Newline : es) $ L.intersperse Newline ops

  exec newInodes printer (entries <> stack)

eval :: Recursive.AlreadySeenInodes -> Printer -> Operation -> IO (Recursive.AlreadySeenInodes, [Operation])
eval inodes (Printer printer) op = case op of
  Newline -> do
    T.putStrLn ""
    pure (inodes, [])
  PrintEntry {..} -> do
    T.putStrLn =<< printer op
    opToOps inodes opt op
    where
      opt = entryOption {Option.level = Depth.decreaseDepth $ Option.level entryOption}
  PrintTree {..} -> do
    T.putStrLn =<< printer op
    opToOps inodes opt op
    where
      opt = entryOption {Option.level = Depth.decreaseDepth $ Option.level entryOption}

opToOps :: Recursive.AlreadySeenInodes -> Option.Option -> Operation -> IO (Recursive.AlreadySeenInodes, [Operation])
opToOps inodes opt op = case op of
  PrintEntry {..}
    | Option.recursive opt && (not . Depth.isDepthZero . Option.level) opt -> mapM (pathToOp opt) paths <&> (newInodes,)
    | otherwise -> pure (Recursive.def, [])
    where
      (nodes, newInodes) = State.runState (Recursive.updateAlreadySeenInode $ filter (Node.isDirectory . Node.pfsNodeType . Node.getNodeStatus) entryNodes) inodes
      paths = map (\node -> entryPath Posix.</> Node.getNodePath node) nodes
  _ -> pure (Recursive.def, [])

pathToOp :: Option.Option -> FilePath -> IO Operation
pathToOp opt path = do
  nodes <- buildDirectoryNodes opt path
  pure $ PrintEntry DIRECTORY path nodes opt

-- | With error message output.
buildDirectoryNodes :: Option.Option -> FilePath -> IO [Node.NodeInfo]
buildDirectoryNodes opt path =
  Utils.listContents opt path >>= \case
    Left errMsg -> IO.hPrint IO.stderr errMsg >> pure []
    Right contents -> Sort.sorter opt <$> (mapM (Node.nodeInfo opt path) . excluder) contents
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
buildInitialOperations :: Option.Option -> [FilePath] -> IO (Recursive.AlreadySeenInodes, [Operation])
buildInitialOperations opt paths = do
  nodeinfos <- mapM (Node.nodeInfo opt "") paths
  let (nodes, inodes) = State.runState (Recursive.updateAlreadySeenInode $ Sort.sorter opt nodeinfos) Recursive.def
  let (dirs, files) = L.partition isDirectory nodes
      fileOp = [PrintEntry FILES "" files opt | not (null files)]
  dirOps <- mapM (pathToOp opt . Node.getNodePath) dirs
  let dirOps' =
        if Option.tree opt
          then map (\PrintEntry {..} -> PrintTree entryPath entryOption) dirOps
          else case dirOps of
            -- Considers no argument to be also a single directory.
            [d] | null files && length (Option.targets opt) < 2 -> [d {entryType = SINGLEDIR}]
            _ -> dirOps
  pure (inodes, L.intersperse Newline $ fileOp <> dirOps')
  where
    isDirectory
      | Option.directory opt = const False
      | otherwise = Node.isDirectory . Node.pfsNodeType . Node.getNodeStatus

buildPrinter :: Option.Option -> Decorator.Printers -> Printer
buildPrinter opt printers = Printer $ fmap (TL.toStrict . TLB.toLazyText . M.mconcat . L.intersperse (TLB.fromText "\n")) . generateEntryLines opt printers

generateEntryLines :: Option.Option -> Decorator.Printers -> Operation -> IO [TLB.Builder]
generateEntryLines opt printers op = case op of
  Newline -> pure []
  PrintEntry {..} -> do
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
            builder = TLB.fromText . T.concat . ("total " :) . map (T.concat . WT.toList) . Size.toTotalBlockSize opt $ map (Node.pfsFileSize . Node.getNodeStatus) entryNodes

    colLen <- Grid.virtualColumnSize opt

    return . addHeader . addTotalBlockSize . Grid.renderGrid $ Grid.buildValidGrid opt colLen nodes'
  PrintTree {..} -> do
    nodes <- Fold.toList <$> Tree.makeTreeNodeInfos opt entryPath
    let opt' = if shouldQuote nodes then opt else opt {Option.noQuote = True}
    pure . map (M.mconcat . map wtToBuilder) $ Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt'
    where
      wtToBuilder wt = M.mconcat . map TLB.fromText $ WT.toList wt

shouldQuote :: [Node.NodeInfo] -> Bool
shouldQuote = not . all (Set.null . Set.intersection setNeedQuotes . Set.fromList . Node.getNodePath)
  where
    setNeedQuotes = Set.fromList Quote.charactorsNeedQuote
