module Haskellorls.Config.Option.When
  ( reader,
    module Haskellorls.Config.When,
  )
where

import Haskellorls.Config.When
import Options.Applicative

reader :: ReadM WHEN
reader =
  str @String >>= \case
    "never" -> return NEVER
    "no" -> return NEVER
    "none" -> return NEVER
    "always" -> return ALWAYS
    "yes" -> return ALWAYS
    "force" -> return ALWAYS
    "auto" -> return AUTO
    "tty" -> return AUTO
    "if-tty" -> return AUTO
    _ -> readerError "Only never, always or auto"
