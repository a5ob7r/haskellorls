module Haskellorls.Indicator.Type
  ( IndicatorStyle (..),
  )
where

data IndicatorStyle
  = IndicatorNone
  | IndicatorFiletype
  | IndicatorSlash
  | IndicatorClassify
  deriving (Eq, Ord)
