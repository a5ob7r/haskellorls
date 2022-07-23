module Haskellorls.Config.Option.Indicator
  ( classifyParser,
    indicatorStyleParser,
    module Haskellorls.Config.Indicator,
  )
where

import Haskellorls.Config.Indicator
import qualified Haskellorls.Config.Option.When as W
import Options.Applicative

classifyParser :: Parser W.WHEN
classifyParser = noArg <|> withArg
  where
    noArg = flag' W.ALWAYS $ long "classify" <> short 'F'
    withArg =
      option W.reader $
        long "classify"
          <> value W.NEVER
          <> metavar "WHEN"
          <> help "Append a indicator follows filename"
          <> completeWith ["never", "always", "auto"]

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
