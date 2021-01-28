module Haskellorls.Grid
  ( terminalColumnSize,
    buildValidGrid,
    renderGrid,
  )
where

import qualified Data.List as List
import qualified Haskellorls.YetAnotherString as YAString
import qualified System.Console.Terminal.Size as TS

terminalColumnSize :: IO Int
terminalColumnSize = maybe 0 TS.width <$> TS.size

nest :: Int -> [a] -> [[a]]
nest n xs
  | n > 0 = nest' m xs
  | otherwise = nest 1 xs
  where
    m = length xs `div` n

nest' :: Int -> [a] -> [[a]]
nest' _ [] = []
nest' 0 xs = nest' 1 xs
nest' n xs = h : nest' n t
  where
    (h, t) = splitAt n xs

buildValidGrid :: Int -> [[YAString.WrapedString]] -> [[[YAString.WrapedString]]]
buildValidGrid columnLength sss
  | columnLength < 1 = buildGrid 1 sss
  | null sss = []
  | otherwise = last . takeWhile (validateGrid columnLength) $ map (`buildGrid` sss) [1..columnLength]

renderGrid :: [[[YAString.WrapedString]]] -> [String]
renderGrid = map renderLine

renderGridAsPlain :: [[[YAString.WrapedString]]] -> [String]
renderGridAsPlain = map renderLineAsPlain

renderLine :: [[YAString.WrapedString]] -> String
renderLine = concatMap YAString.yaShow' . List.intersperse padding
  where
    padding = YAString.toWrappedStringArray gridMargin

renderLineAsPlain :: [[YAString.WrapedString]] -> String
renderLineAsPlain = concatMap YAString.yaShow . List.intersperse padding
  where
    padding = YAString.toWrappedStringArray gridMargin

validateGrid :: Int -> [[[YAString.WrapedString]]] -> Bool
validateGrid n grid
  | n >= maxLen = True
  | otherwise = False
  where
    maxLen = maximum . map length $ renderGridAsPlain grid

buildGrid :: Int -> [[YAString.WrapedString]] -> [[[YAString.WrapedString]]]
buildGrid n = List.transpose . buildGrid' . nest n

buildGrid' :: [[[YAString.WrapedString]]] -> [[[YAString.WrapedString]]]
buildGrid' [] = []
buildGrid' [ss] = [ss]
buildGrid' (ss : sss) = buildColumn ss : buildGrid' sss

buildColumn :: [[YAString.WrapedString]] -> [[YAString.WrapedString]]
buildColumn sss = map (pad paddingChar maxLen) sss
  where
    maxLen = YAString.maximumLength sss

pad :: Char -> Int -> [YAString.WrapedString] -> [YAString.WrapedString]
pad c n ss = ss <> padding
  where
    len = YAString.yaLength ss
    diff = n - len
    padding = YAString.toWrappedStringArray $ replicate diff c

paddingChar :: Char
paddingChar = ' '

gridMargin :: String
gridMargin = "  "
