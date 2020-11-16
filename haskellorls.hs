module Main where

import Data.Maybe (fromMaybe, listToMaybe)

import System.Directory.Extra
import System.IO
import qualified Options.Applicative as OA

import qualified Haskellorls.Color as Color
import Haskellorls.Decorator
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
      nodeNamesForDisplay = if shouldColorize
                               then map (Color.colorizedNodeName cConfig) nodes
                               else map nodeName nodes
  mapM_ (putStrLn . (\(a, b) -> a ++ " " ++ b)) $ zip additionals nodeNamesForDisplay
    where fs = [nodeMode, nodeOwner, nodeGroup, nodeSize, nodeMtime]
