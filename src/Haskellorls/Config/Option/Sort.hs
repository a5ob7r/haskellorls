module Haskellorls.Config.Option.Sort
  ( sortParser,
    module Haskellorls.Config.Sort,
  )
where

import Haskellorls.Config.Sort
import Options.Applicative

sortParser :: Parser SortType
sortParser = option
  do
    str @String >>= \case
      "none" -> pure NONE
      "size" -> pure SIZE
      "time" -> pure TIME
      "version" -> pure VERSION
      "extension" -> pure EXTENSION
      _ -> readerError "Avairable values are only 'none', 'size', 'time', 'version' and 'extension'"
  do
    long "sort"
      <> metavar "WORD"
      <> value NAME
      <> help "Specify an attribute to sort outputs"
      <> completeWith ["none", "size", "time", "version", "extension"]
