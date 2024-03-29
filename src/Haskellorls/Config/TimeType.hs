module Haskellorls.Config.TimeType (TimeType (..)) where

import Control.Exception.Safe (Exception, toException)
import Data.List (intercalate)
import Witch (TryFrom (..), TryFromException (..))

-- TODO: Birth(creation) time is not implemented yet.
data TimeType
  = MODIFICATION
  | ACCESS
  | CHANGE

data TimeTypeException = InvalidFormat

instance Show TimeTypeException where
  show InvalidFormat =
    intercalate
      "\n"
      [ "Invalid time kind format.",
        "",
        "Valid time kind formats are as below.",
        "  - atime, access, use",
        "  - ctime, status",
        "  - mtime, modification",
        "  - birth, creation"
      ]

instance Exception TimeTypeException

instance TryFrom String TimeType where
  tryFrom = \case
    "atime" -> Right ACCESS
    "access" -> Right ACCESS
    "use" -> Right ACCESS
    "ctime" -> Right CHANGE
    "status" -> Right CHANGE
    "mtime" -> Right MODIFICATION
    "modification" -> Right MODIFICATION
    -- Fallback to modification time because we can't probably get file
    -- creation times now.
    "birth" -> Right MODIFICATION
    "creation" -> Right MODIFICATION
    s -> Left . TryFromException s . Just $ toException InvalidFormat
