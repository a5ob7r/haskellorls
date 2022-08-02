module Haskellorls.Config.Option.Format
  ( formatParser,
    module Haskellorls.Config.Format,
  )
where

import Haskellorls.Config.Format
import Options.Applicative

formatParser :: Parser (Maybe Format)
formatParser =
  option reader $
    long "format"
      <> metavar "WORD"
      <> value Nothing
      <> completeWith ["across", "commas", "horizontal", "long", "single-column", "verbose", "vertical"]
  where
    reader =
      str @String >>= \case
        "across" -> pure $ Just HORIZONTAL
        "commas" -> pure $ Just COMMAS
        "horizontal" -> pure $ Just HORIZONTAL
        "long" -> pure $ Just LONG
        "single-column" -> pure $ Just SINGLECOLUMN
        "verbose" -> pure $ Just LONG
        "vertical" -> pure $ Just VERTICAL
        _ -> readerError "Avairable values are only 'across', 'commas', 'horizontal', 'long', 'single-column', 'verbose' and 'vertical'"
