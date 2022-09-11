module Haskellorls.Config.When (WHEN (..), WhenException (..)) where

import Control.Exception.Safe (Exception, toException)
import Data.List (intercalate)
import Witch (TryFrom (..), TryFromException (..))

data WHEN = NEVER | ALWAYS | AUTO

data WhenException = InvalidFormat

instance Show WhenException where
  show InvalidFormat =
    intercalate
      "\n"
      [ "Invalid WHEN format.",
        "",
        "Valid WHEN formats are as below.",
        "  - never",
        "  - always",
        "  - auto"
      ]

instance Exception WhenException

instance TryFrom String WHEN where
  tryFrom = \case
    "never" -> Right NEVER
    "no" -> Right NEVER
    "none" -> Right NEVER
    "always" -> Right ALWAYS
    "yes" -> Right ALWAYS
    "force" -> Right ALWAYS
    "auto" -> Right AUTO
    "tty" -> Right AUTO
    "if-tty" -> Right AUTO
    s -> Left . TryFromException s . Just $ toException InvalidFormat
