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

-- | 'splitInto' @n xs@ returns a list which are sliced @xs@ into @n@ elements:
--
-- > splitInto 2 [1..6] == [[1, 2, 3], [4, 5, 6]]
-- > splitInto 4 [1..8] == [[1, 2], [3, 4], [5, 6], [7, 8]]
-- > splitInto 3 [1..10] == [[1, 2, 3, 4], [5, 6, 7], [8, 9, 10]]
splitInto :: Int -> [a] -> [[a]]
splitInto _ [] = []
splitInto n xs
  | n < 1 = splitInto 1 xs
  | otherwise = slice m xs
  where
    (a, b) = length xs `divMod` n
    m = a + abs (signum b)

-- | 'slice' @n xs@ returns a list which have @n@ length lists as elements:
--
-- > slice 2 [1..6] == [[1, 2], [3, 4], [5, 6]]
-- > slice 2 [1..5] == [[1, 2], [3, 4], [5]]
-- > slice 2 [] == []
-- > slice (-1) [1..5] == []
slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice n xs
  | n < 1 = []
  | otherwise = h : slice n t
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
buildGrid n = List.transpose . buildGrid' . splitInto n

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
