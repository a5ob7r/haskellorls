module Haskellorls.Config.Indicator (IndicatorStyle (..)) where

data IndicatorStyle
  = IndicatorNone
  | IndicatorFiletype
  | IndicatorSlash
  | IndicatorClassify
  deriving (Eq, Ord)
