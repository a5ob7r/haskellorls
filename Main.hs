module Main where

import System.Environment (getArgs)

import System.Directory.Extra

import Haskellorls.Color
import Haskellorls.Decorator
import Haskellorls.Node

main :: IO ()
main = do
  path <- pure head <*> getArgs
  contents <- listContents $ path
  nodes <- mapM node contents
  indicators <- colorIndicators
  let additionals = decorator nodes fs
      names = map name nodes
      namesWithColor = map (\name -> colorize (lookupEscSec indicators name) name) names
  mapM_ putStrLn . map (\(a, b) -> a ++ " " ++ b) $ zip additionals namesWithColor
    where fs = [mode, owner, group, size, mtime]
