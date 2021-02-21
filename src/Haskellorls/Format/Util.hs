module Haskellorls.Format.Util
  ( formatStyle,
    isLongStyle,
    module Haskellorls.Format.Type,
  )
where

import Haskellorls.Format.Type
import qualified Haskellorls.Option as Option

-- WIP: Some format type is not implemented.
formatStyle :: Option.Option -> Format
formatStyle opt
  | False = VERTICAL
  | False = HORIZONTAL
  | False = COMMAS
  | Option.oneline opt = SINGLECOLUMN
  | isLongStyle opt = LONG
  | otherwise = VERTICAL

-- TODO: Should export this?
isLongStyle :: Option.Option -> Bool
isLongStyle opt = any (\f -> f opt) [Option.long, Option.longWithoutGroup, Option.longWithoutOwner, Option.fullTime]
