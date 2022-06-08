module Haskellorls.Sort.Option
  ( sortParser,
    noneSortParser,
    sizeSortParser,
    timeSortParser,
    naturalSortParser,
    extensionSortParser,
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
      <> completeWith ["none", "size", "time", "version", "extension"]
  where
    reader =
      (str :: ReadM String) >>= \case
        "none" -> pure NONE
        "size" -> pure SIZE
        "time" -> pure TIME
        "version" -> pure VERSION
        "extension" -> pure EXTENSION
        _ -> readerError "Avairable values are only 'none', 'size', 'time', 'version' and 'extension'"

noneSortParser :: Parser Bool
noneSortParser =
  switch $
    short 'U'
      <> help "Do not sort"

sizeSortParser :: Parser Bool
sizeSortParser =
  switch $
    short 'S'
      <> help "Size sort, largest first"

timeSortParser :: Parser Bool
timeSortParser =
  switch $
    short 't'
      <> help "Time sort, newest first"

naturalSortParser :: Parser Bool
naturalSortParser =
  switch $
    short 'v'
      <> help "Natural sort"

extensionSortParser :: Parser Bool
extensionSortParser =
  switch $
    short 'X'
      <> help "Alphabetical sort by file extension"
