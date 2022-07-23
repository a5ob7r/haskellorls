module Haskellorls.Config.Sort (SortType (..)) where

data SortType
  = NONE
  | NAME
  | SIZE
  | TIME
  | VERSION
  | EXTENSION
  deriving (Show, Eq, Ord)
