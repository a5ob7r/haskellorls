module Haskellorls.Indicator.Type
  ( WHEN (..),
    IndicatorStyle (..),
  )
where

data WHEN = NEVER | ALWAYS | AUTO

data IndicatorStyle
  = IndicatorNone
  | IndicatorFiletype
  | IndicatorSlash
  | IndicatorClassify
  deriving (Eq, Ord)
