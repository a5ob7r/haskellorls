module Haskellorls.Config.Quote (QuotingStyle (..), QuotingStyleException (..)) where

import Control.Exception.Safe (Exception, toException)
import Data.List (intercalate)
import Witch (TryFrom (..), TryFromException (..))

data QuotingStyle
  = Literal
  | Shell
  | ShellAlways
  | ShellEscape
  | ShellEscapeAlways
  | C
  | Escape
  | CLocale Char Char
  | Locale Char Char

data QuotingStyleException = InvalidFormat

instance Show QuotingStyleException where
  show InvalidFormat =
    intercalate
      "\n"
      [ "Invalid quoting style format.",
        "",
        "Valid quoting style formats are as below.",
        "  - literal",
        "  - shell",
        "  - shell-always",
        "  - shell-escape",
        "  - shell-escape-always",
        "  - c",
        "  - escape",
        "  - clocale",
        "  - locale"
      ]

instance Exception QuotingStyleException

instance TryFrom String QuotingStyle where
  tryFrom = \case
    "literal" -> Right Literal
    "shell" -> Right Shell
    "shell-always" -> Right ShellAlways
    "shell-escape" -> Right ShellEscape
    "shell-escape-always" -> Right ShellEscapeAlways
    "c" -> Right C
    "escape" -> Right Escape
    "clocale" -> Right $ CLocale '`' '\''
    "locale" -> Right $ Locale '`' '\''
    s -> Left . TryFromException s . Just $ toException InvalidFormat
