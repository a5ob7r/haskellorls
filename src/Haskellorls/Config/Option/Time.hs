module Haskellorls.Config.Option.Time
  ( timeParser,
    timeStyleParser,
    module Haskellorls.Config.Time,
  )
where

import Haskellorls.Config.Time
import Options.Applicative

timeParser :: Parser TimeType
timeParser = option
  do
    str @String >>= \case
      "atime" -> pure ACCESS
      "access" -> pure ACCESS
      "use" -> pure ACCESS
      "ctime" -> pure CHANGE
      "status" -> pure CHANGE
      -- "birth" -> BIRTH
      -- "creation" -> BIRTH
      _ -> readerError "Available values are only 'atime', 'access', 'use', 'ctime' and 'status'"
  do
    long "time"
      <> metavar "WORD"
      <> value MODIFICATION
      <> help "Specify a time kind which is used as file's time attribute"
      <> completeWith ["atime", "access", "use", "ctime", "status"]

timeStyleParser :: Parser (Maybe TimeStyle)
timeStyleParser = option
  do str >>= maybe (readerError "Invalid time output format.") (return . Just) . parseTimeStyle
  do
    long "time-style"
      <> metavar "TYPE_STYLE"
      <> value Nothing
      <> help "Specify time output format"
      <> completeWith ["full-iso", "posix-full-iso", "long-iso", "posix-long-iso", "iso", "posix-iso", "locale", "posix-locale"]
