{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.Recursive
  ( buildPrinter,
    buildInitialOperations,
    exec,
    generateEntryLines,
  )
where

import qualified Data.Foldable as Fold
import qualified Data.Functor as F
import qualified Data.List as L
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Depth as Depth
import qualified Haskellorls.Grid as Grid
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Size.Decorator as Size
import qualified Haskellorls.Sort.Method as Sort
import qualified Haskellorls.Tree.Util as Tree
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import qualified System.FilePath.Posix as Posix
import qualified System.Posix.Files as Files

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
exec :: Printer -> [Operation] -> IO ()
exec _ [] = pure ()
exec printer (op : stack) = do
  entries <- L.intersperse Newline <$> eval printer op
  let entries' = if null entries then entries else Newline : entries

  exec printer (entries' <> stack)

eval :: Printer -> Operation -> IO [Operation]
eval (Printer printer) op = case op of
  Newline -> do
    T.putStrLn ""
    pure []
  PrintEntry {..} -> do
    T.putStrLn =<< printer op
    opToOps opt op
    where
      opt = entryOption {Option.level = Depth.decreaseDepth $ Option.level entryOption}
  PrintTree {..} -> do
    T.putStrLn =<< printer op
    opToOps opt op
    where
      opt = entryOption {Option.level = Depth.decreaseDepth $ Option.level entryOption}

opToOps :: Option.Option -> Operation -> IO [Operation]
opToOps opt op = case op of
  PrintEntry {..}
    | Option.recursive opt && not isDepthZero -> mapM (pathToOp opt) paths
    | otherwise -> pure []
    where
      isDepthZero = (Just 0 ==) . Depth.getDepth $ Option.level opt
      paths = map (\node -> entryPath Posix.</> Node.nodeInfoPath node) $ filter (Files.isDirectory . Node.nodeInfoStatus) entryNodes
  _ -> pure []

pathToOp :: Option.Option -> FilePath -> IO Operation
pathToOp opt path = do
  nodes <- buildDirectoryNodes opt path
  pure $ PrintEntry DIRECTORY path nodes opt

buildDirectoryNodes :: Option.Option -> FilePath -> IO [Node.NodeInfo]
buildDirectoryNodes opt path = do
  Utils.listContents opt path >>= mapM (Node.nodeInfo path) . excluder F.<&> Sort.sorter opt
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
buildInitialOperations :: Option.Option -> [FilePath] -> IO [Operation]
buildInitialOperations opt paths = do
  nodes <- Sort.sorter opt <$> mapM (Node.nodeInfo "") paths
  let (dirs, files) = L.partition (Files.isDirectory . Node.nodeInfoStatus) nodes
      fileOp = [PrintEntry FILES "" files opt | not (null files)]
  dirOps <- mapM (pathToOp opt . Node.nodeInfoPath) dirs
  let dirOps' =
        if Option.tree opt
          then map (\PrintEntry {..} -> PrintTree entryPath entryOption) dirOps
          else case dirOps of
            -- Considers no argument to be also a single directory.
            [d] | null files && length (Option.targets opt) < 2 -> [d {entryType = SINGLEDIR}]
            _ -> dirOps
  pure . L.intersperse Newline $ fileOp <> dirOps'

buildPrinter :: Option.Option -> Decorator.Printers -> Printer
buildPrinter opt printers = Printer $ fmap (TL.toStrict . TLB.toLazyText . M.mconcat . L.intersperse (TLB.fromText "\n")) . generateEntryLines opt printers

generateEntryLines :: Option.Option -> Decorator.Printers -> Operation -> IO [TLB.Builder]
generateEntryLines opt printers op = case op of
  Newline -> pure []
  PrintEntry {..} -> do
    let nodes' = Decorator.buildLines entryNodes printers $ Decorator.buildPrinterTypes opt
        addHeader = case entryType of
          FILES -> id
          SINGLEDIR | not (Option.recursive opt) -> id
          _ -> \ss -> TLB.fromText (T.pack entryPath `T.snoc` ':') : ss

        -- Add total block size header only about directries when long style layout.
        addTotalBlockSize = case entryType of
          FILES -> id
          _
            | Decorator.isLongStyle opt -> (builder :)
            | otherwise -> id
            where
              builder = TLB.fromText . T.concat . ("total " :) . map (T.concat . WT.toList) . Size.toTotalBlockSize opt $ map (Files.fileSize . Node.nodeInfoStatus) entryNodes

    colLen <- Grid.virtualColumnSize opt

    return . addHeader . addTotalBlockSize . Grid.renderGrid $ Grid.buildValidGrid colLen nodes'
  PrintTree {..} -> do
    nodes <- Fold.toList <$> Tree.makeTreeNodeInfos opt entryPath
    pure . map (M.mconcat . map wtToBuilder) . Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt
    where
      wtToBuilder wt = M.mconcat . map TLB.fromText $ WT.toList wt
