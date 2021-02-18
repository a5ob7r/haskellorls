{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Grid
  ( virtualColumnSize,
    buildValidGrid,
    renderGrid,
  )
where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Option as Option
import qualified Haskellorls.WrappedText as WT
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

buildValidGrid :: Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildValidGrid _ [] = []
buildValidGrid columnLength sss = case columnLength `compare` 0 of
  EQ -> buildGrid (length sss) sss
  LT -> singleColumnGrid
  GT -> last $ singleColumnGrid : validGrids
  where
    singleColumnGrid = buildGrid 1 sss
    validGrids = takeWhile (validateGrid columnLength) $ map (`buildGrid` sss) [2 .. columnLength]

renderGrid :: [[[WT.WrappedText]]] -> [TLB.Builder]
renderGrid = map renderLine

renderGridAsPlain :: [[[WT.WrappedText]]] -> [TLB.Builder]
renderGridAsPlain = map renderLineAsPlain

renderLine :: [[WT.WrappedText]] -> TLB.Builder
renderLine = M.mconcat . List.intersperse (TLB.fromText gridMargin) . map renderWTList

renderLineAsPlain :: [[WT.WrappedText]] -> TLB.Builder
renderLineAsPlain = M.mconcat . List.intersperse (TLB.fromText gridMargin) . map renderWTListAsPlain

renderWTList :: [WT.WrappedText] -> TLB.Builder
renderWTList = M.mconcat . map (M.mconcat . map TLB.fromText . WT.toList)

renderWTListAsPlain :: [WT.WrappedText] -> TLB.Builder
renderWTListAsPlain = M.mconcat . map (TLB.fromText . WT.wtWord)

validateGrid :: Int -> [[[WT.WrappedText]]] -> Bool
validateGrid n grid
  | n >= maxLen = True
  | otherwise = False
  where
    maxLen = maximum . map (T.length . TL.toStrict . TLB.toLazyText) $ renderGridAsPlain grid

buildGrid :: Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildGrid n = List.transpose . buildGrid' . splitInto n

buildGrid' :: [[[WT.WrappedText]]] -> [[[WT.WrappedText]]]
buildGrid' [] = []
buildGrid' [x] = [x]
buildGrid' xs = mapToInit buildColumn xs

mapToInit :: (a -> a) -> [a] -> [a]
mapToInit f xs = map f (init xs) <> [last xs]

buildColumn :: [[WT.WrappedText]] -> [[WT.WrappedText]]
buildColumn sss = map (pad paddingChar maxLen) sss
  where
    maxLen = maximum $ map (sum . map WT.wtLength) sss

pad :: T.Text -> Int -> [WT.WrappedText] -> [WT.WrappedText]
pad c n ss = ss <> padding
  where
    len = sum $ map WT.wtLength ss
    diff = n - len
    padding = WT.toWrappedTextSingleton $ T.replicate diff c

paddingChar :: T.Text
paddingChar = " "

gridMargin :: T.Text
gridMargin = "  "
