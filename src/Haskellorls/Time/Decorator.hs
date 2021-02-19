{-# LANGUAGE LambdaCase #-}

module Haskellorls.Time.Decorator
  ( timeStyle,
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
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.Option as Option
import Haskellorls.Time.Type
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files

timeStyleFunc :: Format.TimeLocale -> Clock.UTCTime -> TimeStyle -> (Clock.UTCTime -> T.Text)
timeStyleFunc locale time = \case
  FULLISO -> timeStyleFunc' ('+' : fullISOFormat) locale time
  LONGISO -> timeStyleFunc' ('+' : longISOFormat) locale time
  ISO -> timeStyleFunc' ('+' : isoFormat) locale time
  FORMAT fmt -> timeStyleFunc' fmt locale time

coloredTimeStyleFunc :: Color.Config -> Format.TimeLocale -> Clock.UTCTime -> TimeStyle -> (Clock.UTCTime -> [WT.WrappedText])
coloredTimeStyleFunc config locale time style fTime = [Color.toWrappedText config getter timeAsT]
  where
    timeAsT = timeStyleFunc locale time style fTime
    getter = Color.dateEscapeSequence . Color.extensionColorConfig

timeStyleFunc' :: String -> Format.TimeLocale -> Clock.UTCTime -> Clock.UTCTime -> T.Text
timeStyleFunc' fmt locale now fTime = T.pack $ Format.formatTime locale format fTime
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
