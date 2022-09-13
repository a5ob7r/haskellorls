module Haskellorls.Formatter.Time (formatFiletime) where

import Data.Time (TimeLocale, TimeZone, UTCTime, diffUTCTime, formatTime, secondsToNominalDiffTime, utcToZonedTime)
import Haskellorls.Config.TimeStyle
import Witch (From (..), via)

newtype TimeFormat = TimeFormat [String]

instance From TimeFormat [String]

instance From [String] TimeFormat

instance From TimeStyle TimeFormat where
  from = \case
    FULLISO -> TimeFormat ["%F %T.%q %z"]
    LONGISO -> TimeFormat ["%F %R"]
    ISO -> TimeFormat ["%F", "%m-%d %H:%M"]
    FORMAT formats -> TimeFormat formats
    -- These are POSIX-locale timestamp formats.
    _ -> TimeFormat ["%b %e  %Y", "%b %e %H:%M"]

-- TODO: Reduce argument numbers.
formatFiletime :: TimeZone -> TimeLocale -> UTCTime -> TimeStyle -> UTCTime -> String
formatFiletime zone locale now style filetime =
  let oneYear = 31556952 :: Int -- 365.2425 * 24 * 60 * 60
      sixMonth = secondsToNominalDiffTime . fromIntegral $ oneYear `div` 2
      format = case via @TimeFormat style of
        [] -> ""
        [fmt] -> fmt
        fmt1 : fmt2 : _
          | abs (now `diffUTCTime` filetime) < sixMonth -> fmt2
          | otherwise -> fmt1
   in -- FIXME: Maybe 'formatTime' is slow a little bit.
      formatTime locale format $ utcToZonedTime zone filetime
