module Haskellorls.Format.Option
  ( formatParser,
    module Haskellorls.Format.Type,
  )
where

import Haskellorls.Format.Type
import Options.Applicative

formatParser :: Parser Format
formatParser =
  option reader $
    long "format"
      <> metavar "WORD"
      <> value VERTICAL
  where
    reader =
      str >>= \case
        "across" -> pure HORIZONTAL
        "commas" -> pure COMMAS
        "horizontal" -> pure HORIZONTAL
        "long" -> pure LONG
        "single-column" -> pure SINGLECOLUMN
        "verbose" -> pure LONG
        "vertical" -> pure VERTICAL
        _ -> readerError "Avairable values are only 'across', 'commas', 'horizontal', 'long', 'single-column', 'verbose' and 'vertical'"
