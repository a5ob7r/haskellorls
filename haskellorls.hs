module Main where

import Data.Maybe (fromMaybe, listToMaybe)
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
  let path = fromMaybe "." . listToMaybe $ Option.targets opt
  contents <- Entry.listContents opt path
  nodes <- fmap (Sort.sorter opt) . mapM Node.nodeInfo $ contents
  printers <- Decorator.buildPrinters opt
  if Option.long opt
    then do
      mapM_ putStrLn $ Decorator.buildLines nodes printers [Decorator.FILEFIELD, Decorator.FILEOWNER, Decorator.FILEGROUP, Decorator.FILESIZE, Decorator.FILETIME, Decorator.FILENAME]
    else do
      Grid.showNodesWithGridForm nodes $ Decorator.fileNamePrinter printers
