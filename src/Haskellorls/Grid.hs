module Haskellorls.Grid
  ( virtualColumnSize,
    buildValidGrid,
    renderGrid,
  )
where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Option as Option
import qualified Haskellorls.YetAnotherString as YAString
import qualified System.Console.Terminal.Size as TS

virtualColumnSize :: Option.Option -> IO Int
virtualColumnSize opt = do
  termWidth <- Just <$> terminalWidth
  return . head $ Maybe.catMaybes [styleWidth, optWidth, termWidth]
  where
    optWidth = Option.width opt
    long = Decorator.isLongStyle opt
    oneline = Option.oneline opt
    styleWidth =
      if long || oneline
        then Just 1
        else Nothing

terminalWidth :: IO Int
terminalWidth = maybe 1 TS.width <$> TS.size

nest :: Int -> [a] -> [[a]]
nest n xs
  | n > 0 = nest' m xs
  | otherwise = nest 1 xs
  where
    (a, b) = length xs `divMod` n
    m = a + abs (signum b)

nest' :: Int -> [a] -> [[a]]
nest' _ [] = []
nest' 0 xs = nest' 1 xs
nest' n xs = h : nest' n t
  where
    (h, t) = splitAt n xs

buildValidGrid :: Int -> [[YAString.WrapedString]] -> [[[YAString.WrapedString]]]
buildValidGrid columnLength sss
  | null sss = []
  | columnLength == 0 = buildGrid (length sss) sss
  | columnLength < 0 = singleColumnGrid
  | otherwise = last $ singleColumnGrid : validGrids
  where
    singleColumnGrid = buildGrid 1 sss
    validGrids = takeWhile (validateGrid columnLength) $ map (`buildGrid` sss) [2 .. columnLength]

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
