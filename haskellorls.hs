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
  if isLongStyle opt
    then do
      mapM_ putStrLn . Decorator.buildLines nodes printers $ buildPrinterTypes opt
    else do
      Grid.showNodesWithGridForm nodes $ Decorator.fileNamePrinter printers

isLongStyle :: Option.Option -> Bool
isLongStyle opt = or . mapF opt $ [Option.long, Option.oneline, Option.longWithoutGroup]

buildPrinterTypes :: Option.Option -> [Decorator.PrinterType]
buildPrinterTypes opt
  | isLongWithoutGroup = [Decorator.FILEFIELD, Decorator.FILEOWNER, Decorator.FILESIZE, Decorator.FILETIME, Decorator.FILENAME]
  | long = [Decorator.FILEFIELD, Decorator.FILEOWNER, Decorator.FILEGROUP, Decorator.FILESIZE, Decorator.FILETIME, Decorator.FILENAME]
  | oneline = [Decorator.FILENAME]
  | otherwise = []
  where
    isLongWithoutGroup = longWithoutGroup || long && noGroup
    long = Option.long opt
    oneline = Option.oneline opt
    noGroup = Option.noGroup opt
    longWithoutGroup = Option.longWithoutGroup opt

mapF :: a -> [a -> b] -> [b]
mapF _ [] = []
mapF x (f:fs) = f x : mapF x fs
