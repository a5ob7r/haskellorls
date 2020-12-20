module Haskellorls.Time
  ( fileModificationTime
  ) where

import System.Posix.Files(FileStatus, modificationTime)
import System.Posix.Types (EpochTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

fileModificationTime :: FileStatus -> String
fileModificationTime = modifiedTime . modificationTime

modifiedTime :: EpochTime -> String
modifiedTime = iso8601Show . posixSecondsToUTCTime . realToFrac
