module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory.Extra
import System.IO
import qualified Options.Applicative as OA

import qualified Haskellorls.Color as Color
import Haskellorls.Decorator
import qualified Haskellorls.Grid as Grid
import Haskellorls.Node
import qualified Haskellorls.Option as Option

main :: IO ()
main = do
  options <- OA.execParser Option.opts
  run options

run :: Option.Option -> IO ()
run opt = do
  let path = fromMaybe "." . listToMaybe $ Option.targets opt
  contents <- listContents path
  nodes <- mapM node contents
  cConfig <- Color.config
  shouldColorize <- case Option.color opt of
                      Option.NEVER -> return False
                      Option.ALWAYS -> return True
                      Option.AUTO -> hIsTerminalDevice stdout
  let additionals = decorator nodes fs
      nodePrinter = if shouldColorize
                       then Color.colorizedNodeName cConfig
                       else nodeName
      nodeNamesForDisplay = map nodePrinter nodes
  if Option.long opt
     then do
       mapM_ (putStrLn . (\(a, b) -> a ++ " " ++ b)) $ zip additionals nodeNamesForDisplay
     else do
       Grid.showNodesWithGridForm nodes nodePrinter
    where fs = [nodeMode, nodeOwner, nodeGroup, nodeSize, nodeMtime]
