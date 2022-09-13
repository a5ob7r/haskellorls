module Haskellorls.Config.TimeStyle (TimeStyle (..), TimeStyleException (..)) where

import Control.Exception.Safe (Exception, toException)
import Data.List.Extra (intercalate, split)
import Data.Time.Clock
import Haskellorls.System.Locale (LcTime (..))
import Witch (From (..), TryFrom (..), TryFromException (..))

newtype Datetime = Datatime UTCTime
  deriving (Eq, Ord, Show)

data TimeStyle
  = FULLISO
  | POSIXFULLISO
  | LONGISO
  | POSIXLONGISO
  | ISO
  | POSIXISO
  | LOCALE
  | POSIXLOCALE
  | FORMAT [String]
  deriving (Eq, Show)

data TimeStyleException = InvalidFormat

instance Show TimeStyleException where
  show InvalidFormat =
    intercalate
      "\n"
      [ "Invalid time style formats.",
        "",
        "Valid formats are as below.",
        "  - [posix-]full-iso",
        "  - [posix-]long-iso",
        "  - [posix-]iso",
        "  - [posix-]locale",
        "  - +FORMAT (is similar to date(1))"
      ]

instance Exception TimeStyleException

instance TryFrom String TimeStyle where
  tryFrom = \case
    "full-iso" -> Right FULLISO
    "posix-full-iso" -> Right POSIXFULLISO
    "long-iso" -> Right LONGISO
    "posix-long-iso" -> Right POSIXLONGISO
    "iso" -> Right ISO
    "posix-iso" -> Right POSIXISO
    "locale" -> Right LOCALE
    "posix-locale" -> Right POSIXLOCALE
    '+' : s -> Right . FORMAT $ split (== '\n') s
    s -> Left . TryFromException s . Just $ toException InvalidFormat

instance From (TimeStyle, LcTime) TimeStyle where
  from (style, LcTime lcTime) = case style of
    POSIXFULLISO | maybe True (`notElem` ["C", "POSIX"]) lcTime -> FULLISO
    POSIXLONGISO | maybe True (`notElem` ["C", "POSIX"]) lcTime -> LONGISO
    POSIXISO | maybe True (`notElem` ["C", "POSIX"]) lcTime -> ISO
    POSIXLOCALE | maybe True (`notElem` ["C", "POSIX"]) lcTime -> LOCALE
    sty -> sty
