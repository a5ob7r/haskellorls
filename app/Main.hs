module Main (main) where

import Haskellorls
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = getArgs >>= haskellorls >>= exitWith
