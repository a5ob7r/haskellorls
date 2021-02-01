module Main where

import Haskellorls (run)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run
