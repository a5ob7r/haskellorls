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
  , toString
  ) where

import qualified System.Posix.Files as Files
import System.Posix.Types (EpochTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- WIP: Birth(creation) time is not implemented yet.
data TimeType
  = MODIFICATION
  | ACCESS
  | CHANGE
  -- | BIRTH

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

fileTime :: TimeType -> (Files.FileStatus -> EpochTime)
fileTime tType = case tType of
  MODIFICATION -> Files.modificationTime
  ACCESS -> Files.accessTime
  CHANGE -> Files.statusChangeTime

toString :: EpochTime -> String
toString = iso8601Show . posixSecondsToUTCTime . realToFrac
