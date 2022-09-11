module Haskellorls.Config.Format (Format (..), FormatException (..)) where

import Control.Exception.Safe (Exception, toException)
import Data.List (intercalate)
import Witch (TryFrom (..), TryFromException (..))

data Format
  = COMMAS
  | HORIZONTAL
  | LONG
  | SINGLECOLUMN
  | VERTICAL
  deriving (Show, Eq, Ord)

data FormatException = InvalidFormat

instance Show FormatException where
  show InvalidFormat =
    intercalate
      "\n"
      [ "Invalid format style format.",
        "",
        "Valid format style formats are as below.",
        "  - access",
        "  - commas",
        "  - horizontal",
        "  - long",
        "  - single-column",
        "  - verbose",
        "  - vertical"
      ]

instance Exception FormatException

instance TryFrom String Format where
  tryFrom = \case
    "across" -> Right HORIZONTAL
    "commas" -> Right COMMAS
    "horizontal" -> Right HORIZONTAL
    "long" -> Right LONG
    "single-column" -> Right SINGLECOLUMN
    "verbose" -> Right LONG
    "vertical" -> Right VERTICAL
    s -> Left . TryFromException s . Just $ toException InvalidFormat
