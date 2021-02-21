{-# LANGUAGE LambdaCase #-}

module Haskellorls.Time.Decorator
  ( timeStyle,
    timeType,
    fileTime,
    timeStyleFunc,
    coloredTimeStyleFunc,
  )
where

import qualified Data.List.Extra as Extra
import qualified Data.Text as T
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Format as Format
import qualified Data.Time.LocalTime as LClock
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.Option as Option
import Haskellorls.Time.Type
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files

-- TODO: Reduce argument numbers
timeStyleFunc :: LClock.TimeZone -> Format.TimeLocale -> Clock.UTCTime -> TimeStyle -> (Clock.UTCTime -> T.Text)
timeStyleFunc zone locale time = \case
  FULLISO -> timeStyleFunc' ('+' : fullISOFormat) zone locale time
  LONGISO -> timeStyleFunc' ('+' : longISOFormat) zone locale time
  ISO -> timeStyleFunc' ('+' : isoFormat) zone locale time
  FORMAT fmt -> timeStyleFunc' fmt zone locale time

coloredTimeStyleFunc :: Color.Config -> LClock.TimeZone -> Format.TimeLocale -> Clock.UTCTime -> TimeStyle -> (Clock.UTCTime -> [WT.WrappedText])
coloredTimeStyleFunc config zone locale time style fTime = [Color.toWrappedText config getter timeAsT]
  where
    timeAsT = timeStyleFunc zone locale time style fTime
    getter = Color.dateEscapeSequence . Color.extensionColorConfig

timeStyleFunc' :: String -> LClock.TimeZone -> Format.TimeLocale -> Clock.UTCTime -> Clock.UTCTime -> T.Text
timeStyleFunc' fmt zone locale now fTime = T.pack . Format.formatTime locale format $ LClock.utcToZonedTime zone fTime
  where
    format = if fTime `isRecentTimeFrom` now then recent else notRecent
    recent = if not (null formats) then head formats else mainISOFormat
    notRecent = if length formats == 2 then last formats else recent
    formats = parseTimeStyleFormat fmt

fileTime :: TimeType -> (Files.FileStatus -> POSIX.POSIXTime)
fileTime tType = case tType of
  MODIFICATION -> Files.modificationTimeHiRes
  ACCESS -> Files.accessTimeHiRes
  CHANGE -> Files.statusChangeTimeHiRes

timeType :: Option.Option -> TimeType
timeType opt
  | Option.ctime opt = CHANGE
  | Option.atime opt = ACCESS
  | otherwise = Option.time opt

timeStyle :: Option.Option -> TimeStyle
timeStyle opt
  | Option.fullTime opt = FULLISO
  | otherwise = Option.timeStyle opt

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

isRecentTimeFrom :: Clock.UTCTime -> Clock.UTCTime -> Bool
isRecentTimeFrom a b = b > a && Clock.diffUTCTime b a < sixMonth
  where
    sixMonth = Clock.secondsToNominalDiffTime . fromIntegral $ oneYear `div` 2
    oneYear = 31556952 :: Int -- 365.2425 * 24 * 60 * 60

parseTimeStyleFormat :: String -> [String]
parseTimeStyleFormat ('+' : s') = Extra.split (== '\n') s'
parseTimeStyleFormat _ = []
