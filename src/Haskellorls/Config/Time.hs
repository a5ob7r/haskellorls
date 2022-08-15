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
    TimeType (..),
  )
where

import Data.Time.Clock

newtype Datetime = Datatime UTCTime
  deriving (Eq, Ord, Show)

-- WIP: locale and Prefixed with 'posix-' is not implemented yet.
data TimeStyle
  = FULLISO
  | LONGISO
  | ISO
  | FORMAT [String]

-- WIP: Birth(creation) time is not implemented yet.
data TimeType
  = MODIFICATION
  | ACCESS
  | CHANGE
