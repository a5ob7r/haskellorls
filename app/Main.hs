module Main (main) where

import Haskellorls
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= haskellorls
