-- Time style
-- - full-iso
-- - long-iso
-- - iso
-- - locale
-- - +FORMAT
-- - prefixed with 'posix-'
--
-- Time kind
-- - modification time(default)
-- - access time(atime, access, use)
-- - change time(ctime, status)
-- - birth time(birth, creation)

module Haskellorls.Time
  ( timeTypeFrom
  , fileTime
  , timeStyleFrom
  , timeStyleFunc
  , coloredTimeStyleFunc
  ) where

import qualified Foreign.C.Types as CTypes
import qualified Data.List.Extra as Extra
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as Format
import qualified Haskellorls.Color as Color
import qualified Haskellorls.YetAnotherString as YAString

-- WIP: locale and Prefixed with 'posix-' is not implemented yet.
data TimeStyle
  = FULLISO
  | LONGISO
  | ISO
  -- | LOCALE
  | FORMAT String

-- WIP: Birth(creation) time is not implemented yet.
data TimeType
  = MODIFICATION
  | ACCESS
  | CHANGE
  -- | BIRTH

timeStyleFrom :: String -> TimeStyle
timeStyleFrom s = case s of
  "full-iso" -> FULLISO
  "long-iso" -> LONGISO
  "iso" -> ISO
  -- "locale" -> LOCALE
  _ -> FORMAT s

timeTypeFrom :: String -> TimeType
timeTypeFrom s = case s of
  "atime" -> ACCESS
  "access" -> ACCESS
  "use" -> ACCESS
  "ctime" -> CHANGE
  "status" -> CHANGE
  -- "birth" -> BIRTH
  -- "creation" -> BIRTH
  _ -> MODIFICATION

timeStyleFunc :: Format.TimeLocale -> Types.EpochTime -> TimeStyle -> (Types.EpochTime -> String)
timeStyleFunc locale time style = case style of
  FULLISO -> timeStyleFunc' ('+':fullISOFormat) locale time
  LONGISO -> timeStyleFunc' ('+':longISOFormat) locale time
  ISO -> timeStyleFunc' ('+':isoFormat) locale time
  FORMAT fmt -> timeStyleFunc' fmt locale time

coloredTimeStyleFunc :: Color.Config -> Format.TimeLocale -> Types.EpochTime -> TimeStyle -> (Types.EpochTime -> [YAString.WrapedString])
coloredTimeStyleFunc config locale time style fTime =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config dateEscSeq,
        YAString.wrappedStringMain = timeFunc fTime,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    timeFunc = timeStyleFunc locale time style
    dateEscSeq = Color.dateEscapeSequence $ Color.extensionColorConfig config

timeStyleFunc' :: String -> Format.TimeLocale -> Types.EpochTime -> Types.EpochTime -> String
timeStyleFunc' fmt locale now fTime = Format.formatTime locale format $ epoch2UTC fTime
  where
    format = if fTime `isRecentTimeFrom` now then recent else notRecent
    recent = if not (null formats) then head formats else mainISOFormat
    notRecent = if length formats == 2 then last formats else recent
    formats = parseTimeStyleFormat fmt

fileTime :: TimeType -> (Files.FileStatus -> Types.EpochTime)
fileTime tType = case tType of
  MODIFICATION -> Files.modificationTime
  ACCESS -> Files.accessTime
  CHANGE -> Files.statusChangeTime

epoch2UTC :: Types.EpochTime -> Clock.UTCTime
epoch2UTC = POSIX.posixSecondsToUTCTime . realToFrac

isoFormat :: String
isoFormat = mainISOFormat ++ ['\n'] ++ subISOFormat

mainISOFormat :: String
mainISOFormat = "%m-%d %H:%M"

subISOFormat :: String
subISOFormat = "%F"

fullISOFormat :: String
fullISOFormat = "%F %T %z"

longISOFormat :: String
longISOFormat = "%F %R"

isRecentTimeFrom :: Types.EpochTime -> Types.EpochTime -> Bool
isRecentTimeFrom a b = b > a && b - a < sixMonth
  where
    sixMonth = CTypes.CTime $ oneYear `div` 2
    oneYear = 31556952 -- 365.2425 * 24 * 60 * 60

parseTimeStyleFormat :: String -> [String]
parseTimeStyleFormat ('+':s') = Extra.split (== '\n') s'
parseTimeStyleFormat _ = []
