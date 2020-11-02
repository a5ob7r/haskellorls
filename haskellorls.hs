module Main where

import System.Environment (getArgs)

import System.Directory.Extra

import Haskellorls.Color
import Haskellorls.Decorator
import Haskellorls.Node

main :: IO ()
main = do
  path <- head <$> getArgs
  contents <- listContents path
  nodes <- mapM node contents
  indicators <- colorIndicators
  let additionals = decorator nodes fs
      names = map nodeName nodes
      namesWithColor = map (\name -> colorize (lookupEscSec indicators name) name) names
  mapM_ (putStrLn . (\(a, b) -> a ++ " " ++ b)) $ zip additionals namesWithColor
    where fs = [nodeMode, nodeOwner, nodeGroup, nodeSize, nodeMtime]
