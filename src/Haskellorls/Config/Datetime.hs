module Haskellorls.Config.Datetime (Datetime (..)) where

import Data.Time.Clock (UTCTime)

newtype Datetime = Datetime UTCTime
  deriving (Eq, Ord, Show)
