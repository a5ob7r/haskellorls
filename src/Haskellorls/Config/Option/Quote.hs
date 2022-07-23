module Haskellorls.Config.Option.Quote
  ( parseQuotingStyle,
    quotingStyleParser,
    module Haskellorls.Config.Quote,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Quote
import Options.Applicative

quotingStyleParser :: Parser QuotingStyle
quotingStyleParser =
  option reader $
    long "quoting-style"
      <> metavar "WORD"
      <> value NoStyle
      <> help "Specify file name and link name quoting style; this also effects to file name and link name escape style"
      <> completeWith ["literal", "shell", "shell-always", "shell-escape", "shell-escape-always", "c", "escape"]
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
