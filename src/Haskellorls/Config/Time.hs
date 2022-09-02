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

module Haskellorls.Config.Time
  ( Datetime (..),
    TimeStyle (..),
    parseTimeStyle,
    TimeType (..),
  )
where

import Data.List.Extra (split)
import Data.Time.Clock

newtype Datetime = Datatime UTCTime
  deriving (Eq, Ord, Show)

data TimeStyle
  = FULLISO
  | POSIXFULLISO
  | LONGISO
  | POSIXLONGISO
  | ISO
  | POSIXISO
  | LOCALE
  | POSIXLOCALE
  | FORMAT [String]

parseTimeStyle :: String -> Maybe TimeStyle
parseTimeStyle = \case
  "full-iso" -> Just FULLISO
  "posix-full-iso" -> Just POSIXFULLISO
  "long-iso" -> Just LONGISO
  "posix-long-iso" -> Just POSIXLONGISO
  "iso" -> Just ISO
  "posix-iso" -> Just POSIXISO
  "locale" -> Just LOCALE
  "posix-locale" -> Just POSIXLOCALE
  '+' : s -> Just . FORMAT $ split (== '\n') s
  _ -> Nothing

-- WIP: Birth(creation) time is not implemented yet.
data TimeType
  = MODIFICATION
  | ACCESS
  | CHANGE
