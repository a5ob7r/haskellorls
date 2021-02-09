{-# LANGUAGE LambdaCase #-}

module Haskellorls.Time.Option
  ( timeParser,
    timeStyleParser,
    module Haskellorls.Time.Type,
  )
where

import Haskellorls.Time.Type
import Options.Applicative

timeParser :: Parser TimeType
timeParser =
  option reader $
    long "time"
      <> metavar "WORD"
      <> value MODIFICATION
      <> help "Specify a time kind which is used as file's time attribute"
  where
    reader =
      str >>= \case
        "atime" -> pure ACCESS
        "access" -> pure ACCESS
        "use" -> pure ACCESS
        "ctime" -> pure CHANGE
        "status" -> pure CHANGE
        -- "birth" -> BIRTH
        -- "creation" -> BIRTH
        _ -> pure MODIFICATION

timeStyleParser :: Parser TimeStyle
timeStyleParser =
  option reader $
    long "time-style"
      <> metavar "TYPE_STYLE"
      <> value ISO
      <> help "Specify time output format"
  where
    reader =
      str >>= \s -> case s of
        "full-iso" -> return FULLISO
        "long-iso" -> return LONGISO
        "iso" -> return ISO
        -- "locale" -> LOCALE
        _ -> return $ FORMAT s
