module Haskellorls.Config.Option.Hyperlink
  ( hyperlinkParser,
  )
where

import Haskellorls.Config.Option.When
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
