module Haskellorls.Formatter.Number
  ( Config (..),
    formatI,
    formatF,
  )
where

import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Ix (inRange)
import Data.List.Extra (chunksOf, intercalate)
import Haskellorls.Class (From (..))
import qualified System.Locale.LocaleConv as LC
import Text.Read (readMaybe)

-- | A configuration for number format.
data Config = Config
  { decimalPoint :: String,
    thousandsSep :: String,
    grouping :: [Int],
    charMax :: Int
  }

instance From LC.Lconv Config where
  from lconv =
    let decimalPoint = LC.decimalPoint lconv
        thousandsSep = LC.thousandsSep lconv
        grouping = LC.grouping lconv
        charMax = LC.charMax
     in Config {..}

-- | Format an integer.
formatI :: Integral a => Config -> a -> String
formatI config@Config {..} i
  | i < 0 = '-' : formatI config (abs i)
  | otherwise = intercalate thousandsSep . reverse . fmap reverse . group charMax grouping . reverse . show . toInteger $ abs i

-- | Format an floating point number.
formatF :: Real a => Config -> a -> String
formatF config@Config {..} f
  | f < 0 = '-' : formatF config (abs f)
  | otherwise =
      let fmtI s = maybe s (formatI config) $ readMaybe @Int s
          fmtF = \case
            '.' : s -> decimalPoint <> s
            s -> s
       in uncurry (<>) . bimap fmtI fmtF . span isDigit . show @Double . realToFrac $ abs f

group :: Int -> [Int] -> [a] -> [[a]]
group _ _ [] = []
group _ [] l = [l]
group cm [n] l = if inRange (1, cm) n then chunksOf n l else [l]
group cm (n : ns) l = if inRange (1, cm) n then let (xs, ys) = splitAt n l in xs : group cm ns ys else [l]
