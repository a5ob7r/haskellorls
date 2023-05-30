module Haskellorls.Config.Sort (SortType (..), SortTypeException (..)) where

import Control.Exception.Safe (Exception, toException)
import Data.List (intercalate)
import Witch (TryFrom (..), TryFromException (..))

data SortType
  = NONE
  | NAME
  | SIZE
  | TIME
  | VERSION
  | EXTENSION
  | WIDTH
  deriving (Show, Eq, Ord)

data SortTypeException = InvalidFormat

instance Show SortTypeException where
  show InvalidFormat =
    intercalate
      "\n"
      [ "Invalid sort type format.",
        "",
        "Valid sort type formats are as below.",
        "  - none",
        "  - size",
        "  - time",
        "  - version",
        "  - extension",
        "  - width"
      ]

instance Exception SortTypeException

instance TryFrom String SortType where
  tryFrom = \case
    "none" -> Right NONE
    "size" -> Right SIZE
    "time" -> Right TIME
    "version" -> Right VERSION
    "extension" -> Right EXTENSION
    "width" -> Right WIDTH
    s -> Left . TryFromException s . Just $ toException InvalidFormat
