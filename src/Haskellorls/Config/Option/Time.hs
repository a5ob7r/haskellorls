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

timeStyleParser :: Parser (Maybe TimeStyle)
timeStyleParser =
  option reader $
    long "time-style"
      <> metavar "TYPE_STYLE"
      <> value Nothing
      <> help "Specify time output format"
      <> completeWith ["full-iso", "posix-full-iso", "long-iso", "posix-long-iso", "iso", "posix-iso"]
  where
    reader =
      str >>= \case
        "full-iso" -> return $ Just FULLISO
        "posix-full-iso" -> return $ Just POSIXFULLISO
        "long-iso" -> return $ Just LONGISO
        "posix-long-iso" -> return $ Just POSIXLONGISO
        "iso" -> return $ Just ISO
        "posix-iso" -> return $ Just POSIXISO
        -- "locale" -> LOCALE
        '+' : s -> return . Just . FORMAT $ split (== '\n') s
        _ -> readerError "Invalid time output format."
