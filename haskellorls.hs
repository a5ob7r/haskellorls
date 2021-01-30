module Main where

import qualified Data.List as List
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Entry as Entry
import qualified Haskellorls.Grid as Grid
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Sort as Sort
import qualified Options.Applicative as OA

main :: IO ()
main = do
  options <- OA.execParser Option.opts
  run options

run :: Option.Option -> IO ()
run opt = do
  let targets = Option.targets opt
      targets' = if null targets then ["."] else targets
  files <- Entry.buildFiles opt targets'
  printers <- Decorator.buildPrinters opt
  let runner = run' opt printers
      actions = map runner $ Entry.toEntries files
      actions' = List.intersperse (putStrLn "") actions
  sequence_ actions'

run' :: Option.Option -> Decorator.Printers -> Entry.Entry -> IO ()
run' opt printers (Entry.Entry eType path contents) = do
  case eType of
    Entry.FILES -> return ()
    _ -> putStrLn $ path ++ ":"
  nodes <- fmap (Sort.sorter opt) . mapM (Node.nodeInfo path) $ contents
  colLen <- Grid.terminalColumnSize
  let outputs =
        if isLongStyle opt
          then List.transpose [Decorator.buildLines nodes printers $ buildPrinterTypes opt]
          else Grid.buildValidGrid colLen nodes'
        where
          printer = Decorator.fileNamePrinter printers
          nodes' = map printer nodes
  mapM_ putStrLn $ Grid.renderGrid outputs

isLongStyle :: Option.Option -> Bool
isLongStyle opt = any (\f -> f opt) [Option.long, Option.oneline, Option.longWithoutGroup, Option.longWithoutOwner]

buildPrinterTypes :: Option.Option -> [Decorator.PrinterType]
buildPrinterTypes opt
  | isLongWithoutOwnerAndGroup = [Decorator.FILEFIELD, Decorator.FILELINK, Decorator.FILESIZE, Decorator.FILETIME, Decorator.FILENAME]
  | isLongWithoutGroup = [Decorator.FILEFIELD, Decorator.FILELINK, Decorator.FILEOWNER, Decorator.FILESIZE, Decorator.FILETIME, Decorator.FILENAME]
  | isLongWithoutOwner = [Decorator.FILEFIELD, Decorator.FILELINK, Decorator.FILEGROUP, Decorator.FILESIZE, Decorator.FILETIME, Decorator.FILENAME]
  | long = [Decorator.FILEFIELD, Decorator.FILELINK, Decorator.FILEOWNER, Decorator.FILEGROUP, Decorator.FILESIZE, Decorator.FILETIME, Decorator.FILENAME]
  | oneline = [Decorator.FILENAME]
  | otherwise = []
  where
    isLongWithoutOwnerAndGroup = isLongWithoutOwner && (isLongWithoutGroup || noGroup)
    isLongWithoutGroup = longWithoutGroup || long && noGroup
    isLongWithoutOwner = longWithoutOwner
    long = Option.long opt
    oneline = Option.oneline opt
    noGroup = Option.noGroup opt
    longWithoutGroup = Option.longWithoutGroup opt
    longWithoutOwner = Option.longWithoutOwner opt
