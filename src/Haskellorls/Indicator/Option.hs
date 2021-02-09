{-# LANGUAGE LambdaCase #-}

module Haskellorls.Indicator.Option
  ( indicatorStyleParser,
    module Haskellorls.Indicator.Type,
  )
where

import Haskellorls.Indicator.Type
import Options.Applicative

indicatorStyleParser :: Parser IndicatorStyle
indicatorStyleParser =
  option reader $
    long "indicator-style"
      <> metavar "WORD"
      <> value IndicatorNone
      <> help "Specify indicator style, 'none', 'slash', 'file-tyep' and 'classify'"
  where
    reader =
      auto >>= \case
        "none" -> return IndicatorNone
        "slash" -> return IndicatorSlash
        "file-type" -> return IndicatorFiletype
        "classify" -> return IndicatorClassify
        _ -> readerError "Avairable values are only 'none', 'slash', 'file-tyep' and 'classify'"
