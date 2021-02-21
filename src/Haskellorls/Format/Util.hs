module Haskellorls.Format.Util
  ( formatStyle,
    isLongStyle,
    module Haskellorls.Format.Type,
  )
where

import Haskellorls.Format.Type
import qualified Haskellorls.Option as Option

formatStyle :: Option.Option -> Format
formatStyle opt
  | Option.vertical opt = VERTICAL
  | Option.horihontal opt = HORIZONTAL
  | Option.fillWidth opt = COMMAS
  | Option.oneline opt = SINGLECOLUMN
  | any (\f -> f opt) [Option.long, Option.longWithoutGroup, Option.longWithoutOwner, Option.fullTime] = LONG
  | otherwise = VERTICAL

isLongStyle :: Option.Option -> Bool
isLongStyle opt = case formatStyle opt of
  LONG -> True
  _ -> False
