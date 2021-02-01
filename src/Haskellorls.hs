module Haskellorls
  ( run,
  )
where

import qualified Data.List as List
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Entry as Entry
import qualified Haskellorls.Grid as Grid
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Sort as Sort
import qualified Options.Applicative as OA

run :: [String] -> IO ()
run args = do
  options <- argParser args

  if Option.version options
    then showVersion
    else runWith options

runWith :: Option.Option -> IO ()
runWith opt = do
  let targets = Option.targets opt
      targets' = if null targets then ["."] else targets

  files <- Entry.buildFiles opt targets'
  printers <- Decorator.buildPrinters opt

  let runner = runWith' opt printers
      actions = map runner $ Entry.toEntries files
      actions' = List.intersperse (putStrLn "") actions

  sequence_ actions'

runWith' :: Option.Option -> Decorator.Printers -> Entry.Entry -> IO ()
runWith' opt printers (Entry.Entry eType path contents) = do
  nodes <- fmap (Sort.sorter opt) . mapM (Node.nodeInfo path) $ contents
  let nodes' = Decorator.buildLines nodes printers $ Decorator.buildPrinterTypes opt

  colLen <- Grid.virtualColumnSize opt

  case eType of
    Entry.FILES -> return ()
    _ -> putStrLn $ path ++ ":"
  mapM_ putStrLn . Grid.renderGrid $ Grid.buildValidGrid colLen nodes'

argParser :: [String] -> IO Option.Option
argParser args = OA.handleParseResult presult
  where
    presult = OA.execParserPure pprefs pinfo args
    pprefs = OA.prefs OA.helpLongEquals
    pinfo = Option.opts

showVersion :: IO ()
showVersion = putStrLn version

version :: String
version = "v0.1.0.0"
