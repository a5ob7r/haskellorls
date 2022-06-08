module Haskellorls.Color.Option
  ( colorParser,
    extraColorParser,
    module Haskellorls.Color.Type,
  )
where

import Haskellorls.Color.Type
import Options.Applicative

colorParser :: Parser Colorize
colorParser =
  option reader $
    long "color"
      <> metavar "WHEN"
      <> value NEVER
      <> help "When use output with color (default is 'never')"
      <> completeWith ["never", "always", "auto"]
  where
    reader =
      (str :: ReadM String) >>= \case
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

extraColorParser :: Parser Bool
extraColorParser =
  switch $
    long "extra-color"
      <> help "Enable extra coloring which is incompatible for GNU ls."
