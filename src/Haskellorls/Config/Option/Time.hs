module Haskellorls.Config.Option.Time
  ( timeParser,
    timeStyleParser,
    module Haskellorls.Config.Time,
  )
where

import Data.List.Extra (split)
import Haskellorls.Config.Time
import Options.Applicative

timeParser :: Parser TimeType
timeParser =
  option reader $
    long "time"
      <> metavar "WORD"
      <> value MODIFICATION
      <> help "Specify a time kind which is used as file's time attribute"
      <> completeWith ["atime", "access", "use", "ctime", "status"]
  where
    reader =
      str @String >>= \case
        "atime" -> pure ACCESS
        "access" -> pure ACCESS
        "use" -> pure ACCESS
        "ctime" -> pure CHANGE
        "status" -> pure CHANGE
        -- "birth" -> BIRTH
        -- "creation" -> BIRTH
        _ -> readerError "Available values are only 'atime', 'access', 'use', 'ctime' and 'status'"

timeStyleParser :: Parser TimeStyle
timeStyleParser =
  option reader $
    long "time-style"
      <> metavar "TYPE_STYLE"
      <> value ISO
      <> help "Specify time output format"
      <> completeWith ["full-iso", "long-iso", "iso"]
  where
    reader =
      str >>= \case
        "full-iso" -> return FULLISO
        "long-iso" -> return LONGISO
        "iso" -> return ISO
        -- "locale" -> LOCALE
        '+' : s -> return . FORMAT $ split (== '\n') s
        _ -> readerError "Invalid time output format."
