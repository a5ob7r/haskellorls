module Haskellorls.Config.Format (Format (..)) where

data Format
  = COMMAS
  | HORIZONTAL
  | LONG
  | SINGLECOLUMN
  | VERTICAL
  deriving (Show, Eq, Ord)
