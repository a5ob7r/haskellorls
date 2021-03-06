{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Quote.Option
  ( parseQuotingStyle,
    quotingStyleParser,
    module Haskellorls.Quote.Type,
  )
where

import qualified Data.Text as T
import Haskellorls.Quote.Type
import Options.Applicative

quotingStyleParser :: Parser QuotingStyle
quotingStyleParser =
  option reader $
    long "quoting-style"
      <> metavar "WORD"
      <> value NoStyle
      <> help "Specify file name and link name quoting style; this also effects to file name and link name escape style"
  where
    reader =
      str >>= \t -> case parseQuotingStyle t of
        Just s -> pure s
        Nothing -> readerError ""

parseQuotingStyle :: T.Text -> Maybe QuotingStyle
parseQuotingStyle = \case
  "literal" -> Just Literal
  "shell" -> Just Shell
  "shell-always" -> Just ShellAlways
  "shell-escape" -> Just ShellEscape
  "shell-escape-always" -> Just ShellEscapeAlways
  "c" -> Just C
  "escape" -> Just Escape
  _ -> Nothing
