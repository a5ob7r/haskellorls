{-# LANGUAGE LambdaCase #-}

module Haskellorls.Sort.Option
  ( sortParser,
    naturalSortParser,
    module Haskellorls.Sort.Type,
  )
where

import Haskellorls.Sort.Type
import Options.Applicative

sortParser :: Parser SortType
sortParser =
  option reader $
    long "sort"
      <> metavar "WORD"
      <> value NAME
      <> help "Specify an attribute to sort outputs"
  where
    reader =
      str >>= \case
        "none" -> pure NONE
        "size" -> pure SIZE
        "time" -> pure TIME
        "version" -> pure VERSION
        "extension" -> pure EXTENSION
        _ -> readerError "Avairable values are only 'none', 'size', 'time', 'version' and 'extension'"

naturalSortParser :: Parser Bool
naturalSortParser =
  switch $
    short 'v'
      <> help "Natural sort"
