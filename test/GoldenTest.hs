module GoldenTest where

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.Text as T
import Haskellorls
import qualified System.Environment as Env
import Test.Tasty
import Test.Tasty.MGolden

test_example01 :: TestTree
test_example01 = testGroup "example01 with `LS_COLOR` env" . map (buildTestPattern [Env.setEnv "LS_COLORS" lsColor01]) $ allArgPatterns [["--width=80", "test/files/examples01"]]

lsColor01 :: String
lsColor01 = "di=38;5;30:*README=38;5;220;1:*README.rst=38;5;220;1:*README.md=38;5;220;1:*.md=38;5;184:*.rst=38;5;184:"

allArgPatterns :: [[String]] -> [[String]]
allArgPatterns args =
  map (filter $ not . null) $
    args
      >>= combine [[""], ["--color=auto"], ["--color=always"], ["--color=never"]]
      >>= combine [[""], ["--extra-color"]]
      >>= combine [[""], ["-1"]]
      >>= combine [[""], ["-F"], ["-p"], ["--file-type"]]
      >>= combine [[""], ["-a"], ["-A"]]

combine :: [[String]] -> [String] -> [[String]]
combine sss ss = map (ss <>) sss

buildTestPattern :: [IO ()] -> [String] -> TestTree
buildTestPattern actions args = goldenTest desc examplePath $
  do
    sequence_ actions
    opt <- argParser args
    T.pack <$> renderEntriesLines opt
  where
    args' = unwords args
    desc = "With " <> args' `wrapWith` "'"
    exampleName = show . MD5.md5 $ BLC.pack args'
    examplePath = "test/examples/" <> exampleName

wrapWith :: String -> String -> String
wrapWith s a = a <> s <> a
