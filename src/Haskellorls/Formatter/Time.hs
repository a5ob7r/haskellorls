module Haskellorls.Formatter.Time
  ( timeStyleFunc,
    normalColoredTimeStyleFunc,
    coloredTimeStyleFunc,
  )
where

import qualified Data.List.Extra as L
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Haskellorls.Config.Time
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.WrappedText as WT

-- TODO: Reduce argument numbers
timeStyleFunc :: TimeZone -> TimeLocale -> UTCTime -> TimeStyle -> UTCTime -> T.Text
timeStyleFunc zone locale time = \case
  FULLISO -> timeStyleFunc' ('+' : fullISOFormat) zone locale time
  LONGISO -> timeStyleFunc' ('+' : longISOFormat) zone locale time
  ISO -> timeStyleFunc' ('+' : isoFormat) zone locale time
  FORMAT fmt -> timeStyleFunc' fmt zone locale time

normalColoredTimeStyleFunc :: Color.LsColors -> TimeZone -> TimeLocale -> UTCTime -> TimeStyle -> UTCTime -> [WT.WrappedText]
normalColoredTimeStyleFunc lscolors zone locale time style fTime = [Color.toWrappedText lscolors getter timeAsT]
  where
    timeAsT = timeStyleFunc zone locale time style fTime
    getter = Color.normal

-- | A node misc time formatter with the @no@ parameter of the @LS_COLORS@.
coloredTimeStyleFunc :: Color.LsColors -> TimeZone -> TimeLocale -> UTCTime -> TimeStyle -> UTCTime -> [WT.WrappedText]
coloredTimeStyleFunc lscolors zone locale time style fTime = [Color.toWrappedText lscolors getter timeAsT]
  where
    timeAsT = timeStyleFunc zone locale time style fTime
    -- TODO: We have no time type information such as modification, access and
    -- so on at this point. So assume that it is modification time.
    getter = Color.lookup $ Datatime fTime

timeStyleFunc' :: String -> TimeZone -> TimeLocale -> UTCTime -> UTCTime -> T.Text
timeStyleFunc' fmt zone locale now fTime = T.pack . formatTime locale format $ utcToZonedTime zone fTime
  where
    format = if fTime `isRecentTimeFrom` now then recent else notRecent
    recent = if not (null formats) then head formats else mainISOFormat
    notRecent = if length formats == 2 then last formats else recent
    formats = parseTimeStyleFormat fmt

isoFormat :: String
isoFormat = mainISOFormat ++ ['\n'] ++ subISOFormat

mainISOFormat :: String
mainISOFormat = "%m-%d %H:%M"

subISOFormat :: String
subISOFormat = "%F"

fullISOFormat :: String
fullISOFormat = "%F %T.%q %z"

longISOFormat :: String
longISOFormat = "%F %R"

isRecentTimeFrom :: UTCTime -> UTCTime -> Bool
isRecentTimeFrom a b = b > a && diffUTCTime b a < sixMonth
  where
    sixMonth = secondsToNominalDiffTime . fromIntegral $ oneYear `div` 2
    oneYear = 31556952 :: Int -- 365.2425 * 24 * 60 * 60

parseTimeStyleFormat :: String -> [String]
parseTimeStyleFormat ('+' : s') = L.split (== '\n') s'
parseTimeStyleFormat _ = []