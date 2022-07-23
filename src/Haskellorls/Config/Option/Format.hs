module Haskellorls.Config.Option.Format
  ( formatParser,
    module Haskellorls.Config.Format,
  )
where

import Haskellorls.Config.Format
import Options.Applicative

formatParser :: Parser Format
formatParser =
  option reader $
    long "format"
      <> metavar "WORD"
      <> value VERTICAL
      <> completeWith ["across", "commas", "horizontal", "long", "single-column", "verbose", "vertical"]
  where
    reader =
      str @String >>= \case
        "across" -> pure HORIZONTAL
        "commas" -> pure COMMAS
        "horizontal" -> pure HORIZONTAL
        "long" -> pure LONG
        "single-column" -> pure SINGLECOLUMN
        "verbose" -> pure LONG
        "vertical" -> pure VERTICAL
        _ -> readerError "Avairable values are only 'across', 'commas', 'horizontal', 'long', 'single-column', 'verbose' and 'vertical'"
