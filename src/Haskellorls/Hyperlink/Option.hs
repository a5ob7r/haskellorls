module Haskellorls.Hyperlink.Option
  ( hyperlinkParser,
    module Haskellorls.Hyperlink.Type,
  )
where

import Haskellorls.Hyperlink.Type
import Options.Applicative

hyperlinkParser :: Parser WHEN
hyperlinkParser = noArg <|> withArg
  where
    noArg = flag' ALWAYS $ long "hyperlink"
    withArg =
      option reader $
        long "hyperlink"
          <> metavar "WHEN"
          <> value NEVER
          <> help "When embed the hyperlink to the file into the filename."
          <> completeWith ["never", "always", "auto"]
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
