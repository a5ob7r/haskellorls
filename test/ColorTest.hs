module ColorTest where

import System.Environment (setEnv)

import qualified Data.Map.Strict as Map
import Test.Tasty.HUnit

import Haskellorls.Color

unit_colorize :: IO ()
unit_colorize = assertEqual "Return a string which is wrapped with escase sequence" "\^[[38;5;1mCOLORIZE\^[[m" $ colorize "38;5;1" "COLORIZE"

unit_lookupEscSec :: IO ()
unit_lookupEscSec = do
  setEnv "LS_COLORS" "*.hs=38;5;81:*.lhs=38;5;71:"
  indicators <- colorIndicators
  assertEqual "Return a escape sequence param when is passed valid key" "38;5;81" $ lookupEscSec indicators "haskellorls.hs"
  assertEqual "Return '' when indicator doesn't have the key" "" $ lookupEscSec indicators "haskellorls.hss"

unit_colorIndicators :: IO ()
unit_colorIndicators = do
  setEnv "LS_COLORS" "*.hs=38;5;81:*.lhs=38;5;81:"
  indicators <- colorIndicators
  assertEqual "Retrun color indicator map which is generated from 'LS_COLORS'" (Map.fromList [(".HS", "38;5;81"), (".LHS", "38;5;81")]) indicators

unit_filenamePtnEscSec :: IO ()
unit_filenamePtnEscSec = do
  assertEqual "Return a pair which have valid filename pattern key" (Just (".HS", "38;5;81")) $ filenamePtnEscSec ("*.hs", "38;5;81")
  assertEqual "Return Nothing when have invalid filename pattern key" Nothing $ filenamePtnEscSec ("hs", "38;5;81")

unit_filenamePattern :: IO ()
unit_filenamePattern = do
  assertEqual "Return a upperized filename pattern string when is passed valid filename pattern stirng" (Just "README") $ filenamePattern "*readme"
  assertEqual "Return a upperized file extension pattern string when is passed valid filename pattern stirng" (Just ".HS") $ filenamePattern "*.hs"
  assertEqual "Return Nothing when is passed invalid filename pattern stirng which the header charactor is not '*'" Nothing $ filenamePattern ".hs"
  assertEqual "Return Nothing when is passed ' '(space)" Nothing $ filenamePattern ""

unit_makePatternEscapePair :: IO ()
unit_makePatternEscapePair = do
  assertEqual "Return a pair which key and escase sequence params when valid string as key value pair is passed" (Just ("*.hs", "38;5;81")) $ makePatternEscapePair "*.hs=38;5;81"
  assertEqual "Return Nothing when invalid string as key value pair is passed" Nothing $ makePatternEscapePair "*.hs=38;5;81:*.lhs=38;5;81:"

unit_getLSCOLORS :: IO ()
unit_getLSCOLORS = do
  setEnv "LS_COLORS" "*.hs=38;5;81:*.lhs=38;5;81:"
  env <- getLSCOLORS
  assertEqual "Return a string which is fetched from the `LS_COLORS` environment varirble" "*.hs=38;5;81:*.lhs=38;5;81:" env

unit_toUpper :: IO ()
unit_toUpper = assertEqual "Return a string which all letter is uppper case" "HASKELLORLS" $ toUppers "haskellorls"
