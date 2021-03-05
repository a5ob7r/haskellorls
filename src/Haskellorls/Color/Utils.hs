module Haskellorls.Color.Utils
  ( shouldColorize,
  )
where

import Haskellorls.Color.Type
import qualified Haskellorls.Option as Option

shouldColorize :: Option.Option -> Bool
shouldColorize opt = case Option.color opt of
  _ | Option.noneSortExtra opt -> False
  NEVER -> False
  ALWAYS -> True
  AUTO -> Option.toStdout opt
