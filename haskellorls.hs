module Main where

import System.Environment (getArgs)

import System.Directory.Extra

import qualified Haskellorls.Color as Color
import Haskellorls.Decorator
import Haskellorls.Node

main :: IO ()
main = do
  path <- head <$> getArgs
  contents <- listContents path
  nodes <- mapM node contents
  cConfig <- Color.config
  let additionals = decorator nodes fs
      namesWithColor = map (Color.colorizedNodeName cConfig) nodes
  mapM_ (putStrLn . (\(a, b) -> a ++ " " ++ b)) $ zip additionals namesWithColor
    where fs = [nodeMode, nodeOwner, nodeGroup, nodeSize, nodeMtime]
