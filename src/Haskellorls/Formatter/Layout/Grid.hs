module Haskellorls.Formatter.Layout.Grid
  ( buildValidGrid,
    renderGrid,
  )
where

import Data.List (intersperse, transpose)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Haskellorls.Class
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Format as Format
import qualified Haskellorls.Formatter.WrappedText as WT

-- | 'horizontalSplitInto' @n xs@ returns a list which are sliced @xs@ into @n@ elements vertically.
--
-- > horizontalSplitInto 2 [1 .. 6] == [[1, 3, 5], [2, 4, 6]]
-- > horizontalSplitInto 3 [1 .. 10] == [[1, 4, 7, 10], [2, 5, 8], [3, 6, 9]]
verticalSplitInto :: Int -> [a] -> [[a]]
verticalSplitInto n = transpose . slice n

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

-- | 'takeWhileAccumL' @predicate accumulator acc xs@ is similar 'takeWhile'.
-- In addition, this function has an accumulator and an accumulated value,
-- which can be used in predicate statement.
--
-- >>> takeWhileAccumL (\a b -> a + b <= n) (+) 10 [1 .. 10]
-- [1, 2, 3, 4]
takeWhileAccumL :: (a -> b -> Bool) -> (a -> b -> a) -> a -> [b] -> [b]
takeWhileAccumL _ _ _ [] = []
takeWhileAccumL p accumulator acc (x : xs) =
  if p acc x
    then x : takeWhileAccumL p accumulator (accumulator acc x) xs
    else []

buildValidCommaSeparatedGrid :: Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildValidCommaSeparatedGrid n sss = intersperse [WT.deserialize " "] <$> splitsAt validLengths sss'
  where
    sss' = mapToInit (<> [WT.deserialize ","]) sss
    lengths = sum . map termLength <$> sss'
    validLengths = length <$> f lengths

    f [] = []
    f (x : xs) =
      let l = length $ takeWhileAccumL (\acc y -> acc + y + 1 <= n) (\acc y -> acc + y + 1) x xs
       in (x : take l xs) : f (drop l xs)

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
    (ys, xs') = splitAt n xs

buildValidGrid :: Config.Config -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildValidGrid _ _ [] = []
buildValidGrid config columnLength sss =
  case Config.format config of
    Format.COMMAS -> buildValidCommaSeparatedGrid columnLength sss
    _ -> case columnLength `compare` 0 of
      EQ -> gridBuilder config (length sss) sss
      LT -> singleColumnGrid
      GT -> last $ singleColumnGrid : validGrids
  where
    singleColumnGrid = gridBuilder config 1 sss
    validColNum = takeWhile (\n -> validateGrid columnLength $ buildGridWithSpace config n sss) [2 .. columnLength]
    validGrids = map (\n -> gridBuilder config n sss) validColNum
    gridBuilder =
      if Config.tabSeparator config
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
    maxLen = maximum . map (termLength . TL.toStrict . TL.toLazyText) $ renderGridAsPlain grid

buildGridWithTab :: Config.Config -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildGridWithTab config n wtss = map (buildRow (Config.tabSize config) maxLengths) $ transpose grid
  where
    grid = splitter n wtss
    maxLengths = mapToInit (+ 2) $ calcEachRowMaxWTLength grid
    splitter = case Config.format config of
      Format.HORIZONTAL -> verticalSplitInto
      _ -> horizontalSplitInto

buildRow :: Int -> [Int] -> [[WT.WrappedText]] -> [[WT.WrappedText]]
buildRow _ [] _ = []
buildRow _ _ [] = []
buildRow tabSize ns wtss = interpolate wtss $ map (\t -> [WT.deserialize t]) paddings
  where
    lengths = map (sum . map termLength) wtss
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
    lengths = map (sum . map termLength) wtss
    paddings = buildPaddingsWithSpace 0 ns lengths

buildPaddingsWithSpace :: Int -> [Int] -> [Int] -> [T.Text]
buildPaddingsWithSpace _ [] _ = []
buildPaddingsWithSpace _ _ [] = []
buildPaddingsWithSpace len (n : ns) (m : ms) = T.replicate (nLen - mLen) " " : buildPaddingsWithSpace nLen ns ms
  where
    nLen = len + n
    mLen = len + m

buildGridWithSpace :: Config.Config -> Int -> [[WT.WrappedText]] -> [[[WT.WrappedText]]]
buildGridWithSpace config n wtss = map (buildRowWithSpace maxLengths) $ transpose grid
  where
    grid = splitter n wtss
    maxLengths = mapToInit (+ 2) $ calcEachRowMaxWTLength grid
    splitter = case Config.format config of
      Format.HORIZONTAL -> verticalSplitInto
      _ -> horizontalSplitInto

-- | Apply a function to each elements in a list, but exclude the last one.
--
-- > mapToInit (+1) [0..2]
-- [1,2,2]
mapToInit :: (a -> a) -> [a] -> [a]
mapToInit f = foldr (\x acc -> if null acc then [x] else f x : acc) mempty

calcEachRowMaxWTLength :: [[[WT.WrappedText]]] -> [Int]
calcEachRowMaxWTLength = map $ maximum . (0 :) . map (sum . map termLength)
