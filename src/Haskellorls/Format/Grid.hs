module Haskellorls.Format.Grid
  ( virtualColumnSize,
    buildValidGrid,
    renderGrid,
  )
where

import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Haskellorls.Format.Util as Format
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import System.Console.Terminal.Size

virtualColumnSize :: Option.Option -> IO Int
virtualColumnSize opt = do
  fdWidth <- fmap width <$> size
  return . head $ catMaybes [styleWidth, optWidth, fdWidth, Just 1]
  where
    optWidth = Option.width opt
    styleWidth = case Format.formatStyle opt of
      Format.SINGLECOLUMN -> Just 1
      Format.LONG -> Just 1
      _ -> Nothing

-- | 'horizontalSplitInto' @n xs@ returns a list which are sliced @xs@ into @n@ elements vertically.
--
-- > horizontalSplitInto 2 [1 .. 6] == [[1, 3, 5], [2, 4, 6]]
-- > horizontalSplitInto 3 [1 .. 10] == [[1, 4, 7, 10], [2, 5, 8], [3, 6, 9]]
verticalSplitInto :: Int -> [a] -> [[a]]
verticalSplitInto n = L.transpose . slice n

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
    sss' = mapToInit (<> [WT.deserialize commaSuffix]) sss
    lengths = map (sum . map WT.len) sss'
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
    (ys, xs') = L.splitAt n xs

buildValidGrid :: Option.Option -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildValidGrid _ _ [] = []
buildValidGrid opt columnLength sss =
  if Option.fillWidth opt
    then buildValidCommaSeparatedGrid columnLength sss
    else case columnLength `compare` 0 of
      EQ -> gridBuilder opt (length sss) sss
      LT -> singleColumnGrid
      GT -> last $ singleColumnGrid : validGrids
  where
    singleColumnGrid = gridBuilder opt 1 sss
    validColNum = takeWhile (\n -> validateGrid columnLength $ buildGridWithSpace opt n sss) [2 .. columnLength]
    validGrids = map (\n -> gridBuilder opt n sss) validColNum
    gridBuilder =
      if Option.tabSeparator opt
        then buildGridWithTab
        else buildGridWithSpace

renderGrid :: [[[WT.WrappedText]]] -> [TL.Builder]
renderGrid = map renderLine

renderGridAsPlain :: [[[WT.WrappedText]]] -> [TL.Builder]
renderGridAsPlain = map renderLineAsPlain

renderLine :: [[WT.WrappedText]] -> TL.Builder
renderLine = foldMap renderWTList

renderLineAsPlain :: [[WT.WrappedText]] -> TL.Builder
renderLineAsPlain = foldMap renderWTListAsPlain

renderWTList :: [WT.WrappedText] -> TL.Builder
renderWTList = foldMap (TL.fromText . WT.serialize)

renderWTListAsPlain :: [WT.WrappedText] -> TL.Builder
renderWTListAsPlain = foldMap (TL.fromText . WT.wtWord)

validateGrid :: Int -> [[[WT.WrappedText]]] -> Bool
validateGrid n grid
  | n >= maxLen = True
  | otherwise = False
  where
    maxLen = maximum . map (Utils.textLengthForDisplay . TL.toStrict . TL.toLazyText) $ renderGridAsPlain grid

buildGridWithTab :: Option.Option -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildGridWithTab opt n wtss = map (buildRow (Option.tabSize opt) maxLengths) $ L.transpose grid
  where
    grid = splitter n wtss
    maxLengths = mapToInit (+ 2) $ calcEachRowMaxWTLength grid
    splitter = case Format.formatStyle opt of
      Format.HORIZONTAL -> verticalSplitInto
      _ -> horizontalSplitInto

buildRow :: Int -> [Int] -> [[WT.WrappedText]] -> [[WT.WrappedText]]
buildRow _ [] _ = []
buildRow _ _ [] = []
buildRow tabSize ns wtss = interpolate wtss $ map (\t -> [WT.deserialize t]) paddings
  where
    lengths = map (sum . map WT.len) wtss
    paddings = buildPaddings tabSize ns lengths

interpolate :: [a] -> [a] -> [a]
interpolate [] _ = []
interpolate [x] _ = [x]
interpolate _ [] = []
interpolate (x : xs) (y : ys) = x : y : interpolate xs ys

buildPaddings :: Int -> [Int] -> [Int] -> [T.Text]
buildPaddings _ [] _ = []
buildPaddings _ _ [] = []
buildPaddings _ [_] _ = []
buildPaddings _ _ [_] = []
buildPaddings tabSize ns ms = buildPaddings' tabSize 0 ns ms

buildPaddings' :: Int -> Int -> [Int] -> [Int] -> [T.Text]
buildPaddings' _ _ _ [] = []
buildPaddings' _ _ [] _ = []
buildPaddings' tabSize len (n : ns) (m : ms) = T.replicate nTabs "\t" <> T.replicate nSps " " : buildPaddings' tabSize nLen ns ms
  where
    nLen = len + n
    mLen = len + m
    (a, b) = nLen `divMod` tabSize
    (a', b') = mLen `divMod` tabSize
    nTabs = a - a'
    nSps = if a == a' then b - b' else b

buildRowWithSpace :: [Int] -> [[WT.WrappedText]] -> [[WT.WrappedText]]
buildRowWithSpace [] _ = []
buildRowWithSpace _ [] = []
buildRowWithSpace ns wtss = interpolate wtss $ map (\t -> [WT.deserialize t]) paddings
  where
    lengths = map (sum . map WT.len) wtss
    paddings = buildPaddingsWithSpace 0 ns lengths

buildPaddingsWithSpace :: Int -> [Int] -> [Int] -> [T.Text]
buildPaddingsWithSpace _ [] _ = []
buildPaddingsWithSpace _ _ [] = []
buildPaddingsWithSpace len (n : ns) (m : ms) = T.replicate (nLen - mLen) " " : buildPaddingsWithSpace nLen ns ms
  where
    nLen = len + n
    mLen = len + m

buildGridWithSpace :: Option.Option -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildGridWithSpace opt n wtss = map (buildRowWithSpace maxLengths) $ L.transpose grid
  where
    grid = splitter n wtss
    maxLengths = mapToInit (+ 2) $ calcEachRowMaxWTLength grid
    splitter = case Format.formatStyle opt of
      Format.HORIZONTAL -> verticalSplitInto
      _ -> horizontalSplitInto

-- | Apply a function to each elements in a list, but exclude the last one.
--
-- > mapToInit (+1) [0..2]
-- [1,2,2]
mapToInit :: (a -> a) -> [a] -> [a]
mapToInit f = foldr (\x acc -> if null acc then [x] else f x : acc) mempty

calcEachRowMaxWTLength :: [[[WT.WrappedText]]] -> [Int]
calcEachRowMaxWTLength = map $ maximum . (0 :) . map (sum . map WT.len)

commaSuffix :: T.Text
commaSuffix = ", "
