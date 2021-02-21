{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Format.Grid
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
import qualified Haskellorls.Format.Util as Format
import qualified Haskellorls.Option as Option
import qualified Haskellorls.WrappedText as WT
import qualified System.Console.Terminal.Size as TS

virtualColumnSize :: Option.Option -> IO Int
virtualColumnSize opt = do
  termWidth <- Just <$> terminalWidth
  return . head $ Maybe.catMaybes [styleWidth, optWidth, termWidth]
  where
    optWidth = Option.width opt
    styleWidth = case Format.formatStyle opt of
      Format.SINGLECOLUMN -> Just 1
      Format.LONG -> Just 1
      _ -> Nothing

terminalWidth :: IO Int
terminalWidth = maybe 1 TS.width <$> TS.size

-- | 'horizontalSplitInto' @n xs@ returns a list which are sliced @xs@ into @n@ elements vertically.
--
-- > horizontalSplitInto 2 [1 .. 6] == [[1, 3, 5], [2, 4, 6]]
-- > horizontalSplitInto 3 [1 .. 10] == [[1, 4, 7, 10], [2, 5, 8], [3, 6, 9]]
verticalSplitInto :: Int -> [a] -> [[a]]
verticalSplitInto n = List.transpose . slice n

horizontalSplitInto :: Int -> [a] -> [[a]]
horizontalSplitInto = splitInto

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

splitIntoMaxSum :: Int -> [Int] -> [[Int]]
splitIntoMaxSum _ [] = []
splitIntoMaxSum n xs = ys : splitIntoMaxSum n xs'
  where
    ys = takeWhileWithAccum (\a b -> a + b <= n) (+) 0 xs
    xs' = drop (length ys) xs

-- | 'takeWhileWithAccum' @predicate accumulator a0 xs@ is similar 'takeWhile'.
-- But this has accumulator and the accumulated value can be used in predicate
-- statement.
--
-- > takeWhileWithAccum (\a b -> a + b <= n) (+) 10 [1 .. 10] == [1, 2, 3, 4]
takeWhileWithAccum :: (a -> b -> Bool) -> (a -> b -> a) -> a -> [b] -> [b]
takeWhileWithAccum _ _ _ [] = []
takeWhileWithAccum p accumulator a0 (x : xs) =
  if p a0 x
    then x : takeWhileWithAccum p accumulator (accumulator a0 x) xs
    else []

buildValidCommaSeparatedGrid :: Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildValidCommaSeparatedGrid n sss = splitsAt validLengths sss'
  where
    sss' = mapToInit (<> WT.toWrappedTextSingleton commaSuffix) sss
    lengths = map (sum . map WT.wtLength) sss'
    validLengths = map length $ splitIntoMaxSum n lengths

-- | 'splitsAt' @ns xs@ returns splited list which has lists as elements. The
-- element's length may be not same.
--
-- > splitsAt [1 .. 3] [1 .. 6] = [[1], [2, 3], [4, 5, 6]]
-- > splitsAt [3, 3 ..] [1 .. 6] = [[1, 2, 3], [4, 5, 6]]
splitsAt :: [Int] -> [a] -> [[a]]
splitsAt [] _ = []
splitsAt _ [] = []
splitsAt (n : nx) xs = ys : splitsAt nx xs'
  where
    (ys, xs') = List.splitAt n xs

buildValidGrid :: Option.Option -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildValidGrid _ _ [] = []
buildValidGrid opt columnLength sss =
  if Option.fillWidth opt
    then buildValidCommaSeparatedGrid columnLength sss
    else case columnLength `compare` 0 of
      EQ -> buildGrid opt (length sss) sss
      LT -> singleColumnGrid
      GT -> last $ singleColumnGrid : validGrids
  where
    singleColumnGrid = buildGrid opt 1 sss
    validGrids = takeWhile (validateGrid columnLength) $ map (\n -> buildGrid opt n sss) [2 .. columnLength]

renderGrid :: [[[WT.WrappedText]]] -> [TLB.Builder]
renderGrid = map renderLine

renderGridAsPlain :: [[[WT.WrappedText]]] -> [TLB.Builder]
renderGridAsPlain = map renderLineAsPlain

renderLine :: [[WT.WrappedText]] -> TLB.Builder
renderLine = M.mconcat . map renderWTList

renderLineAsPlain :: [[WT.WrappedText]] -> TLB.Builder
renderLineAsPlain = M.mconcat . map renderWTListAsPlain

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

buildGrid :: Option.Option -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildGrid opt n = map (List.intersperse (WT.toWrappedTextSingleton gridMargin)) . List.transpose . buildGrid' . splitter n
  where
    splitter = case Format.formatStyle opt of
      Format.HORIZONTAL -> verticalSplitInto
      _ -> horizontalSplitInto

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

commaSuffix :: T.Text
commaSuffix = ", "
