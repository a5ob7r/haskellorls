module ColorTest where

import System.Environment (setEnv)

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath.Posix ((</>))
import Test.Tasty.HUnit (assertEqual)

import Haskellorls.Color
import Haskellorls.Node

tempNode :: FilePath -> FilePath -> IO Node
tempNode directoryPath filename = do
  let filepath = directoryPath </> filename
  writeFile filepath ""
  node filepath

unit_colorizedNodeName :: IO ()
unit_colorizedNodeName = do
  setEnv "LS_COLORS" "*.hs=38;5;81:*.lhs=38;5;71:"
  conf <- config
  withSystemTempDirectory "" $ \directoryPath -> do
    nd1 <- tempNode directoryPath "haskellorls.hs"
    nd2 <- tempNode directoryPath "haskellorls.lhs"
    nd3 <- tempNode directoryPath "haskellorls"

    assertEqual "Return a string which is wrapped with haskell escase sequence" "\^[[38;5;81mhaskellorls.hs\^[[m" $ colorizedNodeName conf nd1
    assertEqual "Return a string which is wrapped with literal haskell escase sequence" "\^[[38;5;71mhaskellorls.lhs\^[[m" $ colorizedNodeName conf nd2
    assertEqual "Return a string which is wrapped with void escase sequence" "\^[[mhaskellorls\^[[m" $ colorizedNodeName conf nd3
