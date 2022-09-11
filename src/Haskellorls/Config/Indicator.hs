module Haskellorls.Config.Indicator (IndicatorStyle (..), IndicatorStyleException (..)) where

import Control.Exception.Safe (Exception, toException)
import Data.List (intercalate)
import Witch (TryFrom (..), TryFromException (..))

data IndicatorStyle
  = IndicatorNone
  | IndicatorFiletype
  | IndicatorSlash
  | IndicatorClassify
  deriving (Eq, Ord)

data IndicatorStyleException = InvalidFormat

instance Show IndicatorStyleException where
  show InvalidFormat =
    intercalate
      "\n"
      [ "Invalid indicator style format.",
        "",
        "Valid indicator style formats are as below.",
        "  - none",
        "  - slash",
        "  - file-style",
        "  - classify"
      ]

instance Exception IndicatorStyleException

instance TryFrom String IndicatorStyle where
  tryFrom = \case
    "none" -> Right IndicatorNone
    "slash" -> Right IndicatorSlash
    "file-type" -> Right IndicatorFiletype
    "classify" -> Right IndicatorClassify
    s -> Left . TryFromException s . Just $ toException InvalidFormat
