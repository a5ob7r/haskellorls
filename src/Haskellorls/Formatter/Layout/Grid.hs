module Haskellorls.Formatter.Layout.Grid (mkValidGrid) where

import Data.List (find, intersperse, transpose, unfoldr)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Haskellorls.Class (TerminalLength (..))
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Format as Format
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import Witch (From (..))

-- | 'splitInto' @n xs@ returns a list which are sliced @xs@ into @n@ elements:
--
-- > splitInto 2 [1..6] == [[1, 2, 3], [4, 5, 6]]
-- > splitInto 4 [1..8] == [[1, 2], [3, 4], [5, 6], [7, 8]]
-- > splitInto 3 [1..10] == [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10]]
splitInto :: Int -> [a] -> [[a]]
splitInto n xs = slice m xs
  where
    (a, b) = length xs `divMod` max n 1
    m = a + abs (signum b)

-- | 'slice' @n xs@ returns a list which have @n@ length lists as elements:
--
-- > slice 2 [1..6] == [[1, 2], [3, 4], [5, 6]]
-- > slice 2 [1..5] == [[1, 2], [3, 4], [5]]
-- > slice 2 [] == []
-- > slice (-1) [1..5] == []
slice :: Int -> [a] -> [[a]]
slice n = unfoldr $ \xs -> let tpl = splitAt n xs in if null $ fst tpl then Nothing else Just tpl

mkValidCommaSeparatedGrid :: Int -> [[Attr.Attribute WT.WrappedText]] -> [[[Attr.Attribute WT.WrappedText]]]
mkValidCommaSeparatedGrid n sss = intersperse [Attr.Other $ from @T.Text " "] <$> splitsAt validLengths sss'
  where
    sss' = mapToInit (<> [Attr.Other $ from @T.Text ","]) sss
    lengths = sum . map (termLength . Attr.unwrap) <$> sss'
    validLengths = length <$> f lengths

    f [] = []
    f (x : xs) =
      let l = length . flip unfoldr (x, xs) $ \case
            (_, []) -> Nothing
            (acc, y : ys) ->
              let acc' = acc + y + 1
               in if acc' <= n
                    then Just (acc', (acc', ys))
                    else Nothing
       in (x : take l xs) : f (drop l xs)

-- | 'splitsAt' @ns xs@ returns splited list which has lists as elements. The
-- element's length may be not same.
--
-- > splitsAt [1 .. 3] [1 .. 6] = [[1], [2, 3], [4, 5, 6]]
-- > splitsAt [3, 3 ..] [1 .. 6] = [[1, 2, 3], [4, 5, 6]]
splitsAt :: [Int] -> [a] -> [[a]]
splitsAt [] _ = []
splitsAt _ [] = []
splitsAt (n : ns) xs = ys : splitsAt ns xs'
  where
    (ys, xs') = splitAt n xs

mkValidGrid :: Config.Config -> Int -> [[Attr.Attribute WT.WrappedText]] -> [[[Attr.Attribute WT.WrappedText]]]
mkValidGrid _ _ [] = []
mkValidGrid config columnLength sss =
  case Config.format config of
    Format.COMMAS -> mkValidCommaSeparatedGrid columnLength sss
    Format.LONG -> mkGrid config 1 sss
    Format.SINGLECOLUMN -> mkGrid config 1 sss
    _
      | columnLength <= 0 -> mkGrid config (length sss) sss
      | otherwise -> fromMaybe (mkGrid config 1 sss) . find (validateGrid columnLength) $ (\n -> mkGrid config n sss) <$> reverse [2 .. length sss]

validateGrid :: Int -> [[[Attr.Attribute WT.WrappedText]]] -> Bool
validateGrid n grid =
  let m = maximum . (0 :) $ (\sss -> sum $ (\ss -> sum $ termLength . Attr.unwrap <$> ss) <$> sss) <$> grid
   in n >= m

interpolate :: [a] -> [a] -> [a]
interpolate [] _ = []
interpolate [x] _ = [x]
interpolate _ [] = []
interpolate (x : xs) (y : ys) = x : y : interpolate xs ys

mkRowWithTab :: Int -> [Int] -> [[Attr.Attribute WT.WrappedText]] -> [[Attr.Attribute WT.WrappedText]]
mkRowWithTab _ [] _ = []
mkRowWithTab _ _ [] = []
mkRowWithTab tabSize ns wtss = interpolate wtss $ (\t -> [Attr.Other $ from t]) <$> paddings
  where
    lengths = sum . map (termLength . Attr.unwrap) <$> wtss
    paddings = f 0 $ zip ns lengths

    f _ [] = []
    f _ [_] = []
    f l ((n, m) : nms) =
      let nLen = l + n
          mLen = l + m
          (a, b) = nLen `divMod` tabSize
          (a', b') = mLen `divMod` tabSize
          nTabs = a - a'
          nSps = if a == a' then b - b' else b
       in T.replicate nTabs "\t" <> T.replicate nSps " " : f nLen nms

mkRowWithSpace :: [Int] -> [[Attr.Attribute WT.WrappedText]] -> [[Attr.Attribute WT.WrappedText]]
mkRowWithSpace [] _ = []
mkRowWithSpace _ [] = []
mkRowWithSpace ns wtss = interpolate wtss $ (\t -> [Attr.Other $ from t]) <$> paddings
  where
    lengths = sum . map (termLength . Attr.unwrap) <$> wtss
    paddings = zipWith (\n m -> T.replicate (n - m) " ") ns lengths

mkGrid :: Config.Config -> Int -> [[Attr.Attribute WT.WrappedText]] -> [[[Attr.Attribute WT.WrappedText]]]
mkGrid config n wtss = case Config.format config of
  Format.HORIZONTAL ->
    let grid = slice n wtss
        m = mapToInit (+ 2) $ maxColumnLength <$> transpose grid
     in mkRow m <$> grid
  _ ->
    let grid = splitInto n wtss
        m = mapToInit (+ 2) $ maxColumnLength <$> grid
     in mkRow m <$> transpose grid
  where
    mkRow = maybe mkRowWithSpace mkRowWithTab $ Config.tabSize config
    maxColumnLength = maximum . (0 :) . map (sum . map (termLength . Attr.unwrap))

-- | Apply a function to each elements in a list, but exclude the last one.
--
-- > mapToInit (+1) [0..2]
-- [1,2,2]
mapToInit :: (a -> a) -> [a] -> [a]
mapToInit f = foldr (\x acc -> if null acc then [x] else f x : acc) mempty
