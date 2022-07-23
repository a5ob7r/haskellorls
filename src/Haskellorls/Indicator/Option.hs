module Haskellorls.Indicator.Option
  ( classifyParser,
    indicatorStyleParser,
    module Haskellorls.Indicator.Type,
  )
where

import Haskellorls.Indicator.Type
import Options.Applicative

classifyParser :: Parser WHEN
classifyParser = noArg <|> withArg
  where
    noArg = flag' ALWAYS $ long "classify" <> short 'F'
    withArg =
      option reader $
        long "classify"
          <> value NEVER
          <> metavar "WHEN"
          <> help "Append a indicator follows filename"
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

indicatorStyleParser :: Parser IndicatorStyle
indicatorStyleParser =
  option reader $
    long "indicator-style"
      <> metavar "WORD"
      <> value IndicatorNone
      <> help "Specify indicator style, 'none', 'slash', 'file-tyep' and 'classify'"
      <> completeWith ["none", "slash", "file-type", "classify"]
  where
    reader =
      str @String >>= \case
        "none" -> return IndicatorNone
        "slash" -> return IndicatorSlash
        "file-type" -> return IndicatorFiletype
        "classify" -> return IndicatorClassify
        _ -> readerError "Avairable values are only 'none', 'slash', 'file-tyep' and 'classify'"
