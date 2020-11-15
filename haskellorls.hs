module Main where

import Data.Maybe (fromMaybe, listToMaybe)

import System.Directory.Extra
import qualified Options.Applicative as OA

import qualified Haskellorls.Color as Color
import Haskellorls.Decorator
import Haskellorls.Node
import qualified Haskellorls.Option as Option

main :: IO ()
main = do
  options <- OA.execParser Option.opts
  let path = fromMaybe "." . listToMaybe $ Option.targets options
  contents <- listContents path
  nodes <- mapM node contents
  cConfig <- Color.config
  let additionals = decorator nodes fs
      namesWithColor = map (Color.colorizedNodeName cConfig) nodes
  mapM_ (putStrLn . (\(a, b) -> a ++ " " ++ b)) $ zip additionals namesWithColor
    where fs = [nodeMode, nodeOwner, nodeGroup, nodeSize, nodeMtime]
