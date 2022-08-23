module Haskellorls.Formatter.Time
  ( timeStyleFunc,
    normalColoredTimeStyleFunc,
    coloredTimeStyleFunc,
  )
where

import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, secondsToNominalDiffTime)
import Data.Time.Format (TimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, utcToZonedTime)
import Haskellorls.Config.Time
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color

-- TODO: Reduce argument numbers
timeStyleFunc :: TimeZone -> TimeLocale -> UTCTime -> Maybe TimeStyle -> UTCTime -> T.Text
timeStyleFunc zone locale time = \case
  Just FULLISO -> formatFiletime ["%F %T.%q %z"] zone locale time
  Just LONGISO -> formatFiletime ["%F %R"] zone locale time
  Just ISO -> formatFiletime ["%F", "%m-%d %H:%M"] zone locale time
  Just (FORMAT formats) -> formatFiletime formats zone locale time
  Nothing -> formatFiletime ["%b %e  %Y", "%b %e %H:%M"] zone locale time

normalColoredTimeStyleFunc :: Color.LsColors -> TimeZone -> TimeLocale -> UTCTime -> Maybe TimeStyle -> UTCTime -> [Attr.Attribute WT.WrappedText]
normalColoredTimeStyleFunc lscolors zone locale time style fTime = [Attr.Other $ WT.wrap lscolors getter timeAsT]
  where
    timeAsT = timeStyleFunc zone locale time style fTime
    getter = Color.normal

-- | A node misc time formatter with the @no@ parameter of the @LS_COLORS@.
coloredTimeStyleFunc :: Color.LsColors -> TimeZone -> TimeLocale -> UTCTime -> Maybe TimeStyle -> UTCTime -> [Attr.Attribute WT.WrappedText]
coloredTimeStyleFunc lscolors zone locale time style fTime = [Attr.Other $ WT.wrap lscolors getter timeAsT]
  where
    timeAsT = timeStyleFunc zone locale time style fTime
    -- TODO: We have no time type information such as modification, access and
    -- so on at this point. So assume that it is modification time.
    getter = Color.lookup $ Datatime fTime

-- FIXME: Maybe 'formatTime' is slow a little bit.
formatFiletime :: [String] -> TimeZone -> TimeLocale -> UTCTime -> UTCTime -> T.Text
formatFiletime formats zone locale now filetime = T.pack . formatTime locale format $ utcToZonedTime zone filetime
  where
    format = case formats of
      [] -> ""
      [fmt] -> fmt
      fmt1 : fmt2 : _
        | abs (now `diffUTCTime` filetime) < sixMonth -> fmt2
        | otherwise -> fmt1
    sixMonth = secondsToNominalDiffTime . fromIntegral $ oneYear `div` 2
    oneYear = 31556952 :: Int -- 365.2425 * 24 * 60 * 60
