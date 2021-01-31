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
  options <- parser Option.opts
  if Option.version options
    then showVersion
    else run options
  where
    parser = OA.customExecParser $ OA.prefs OA.helpLongEquals

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
  colLen <- Grid.virtualColumnSize opt
  let nodes' = Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt
  mapM_ putStrLn . Grid.renderGrid $ Grid.buildValidGrid colLen nodes'

showVersion :: IO ()
showVersion = putStrLn version

version :: String
version = "v0.1.0.0"
