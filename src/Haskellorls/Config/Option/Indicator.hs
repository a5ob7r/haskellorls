module Haskellorls.Config.Option.Indicator
  ( indicatorStyleParser,
    module Haskellorls.Config.Indicator,
  )
where

import Haskellorls.Config.Indicator
import Options.Applicative

indicatorStyleParser :: Parser IndicatorStyle
indicatorStyleParser = option
  do
    str @String >>= \case
      "none" -> return IndicatorNone
      "slash" -> return IndicatorSlash
      "file-type" -> return IndicatorFiletype
      "classify" -> return IndicatorClassify
      _ -> readerError "Avairable values are only 'none', 'slash', 'file-tyep' and 'classify'"
  do
    long "indicator-style"
      <> metavar "WORD"
      <> value IndicatorNone
      <> help "Specify indicator style, 'none', 'slash', 'file-tyep' and 'classify'"
      <> completeWith ["none", "slash", "file-type", "classify"]
