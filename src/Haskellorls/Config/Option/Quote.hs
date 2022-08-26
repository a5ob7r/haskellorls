module Haskellorls.Config.Option.Quote
  ( parseQuotingStyle,
    quotingStyleParser,
    module Haskellorls.Config.Quote,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Quote
import Options.Applicative

quotingStyleParser :: Parser (Maybe QuotingStyle)
quotingStyleParser = option
  do str >>= maybe (readerError "") (return . Just) . parseQuotingStyle
  do
    long "quoting-style"
      <> metavar "WORD"
      <> value Nothing
      <> help "Specify file name and link name quoting style; this also effects to file name and link name escape style"
      <> completeWith ["literal", "shell", "shell-always", "shell-escape", "shell-escape-always", "c", "escape", "clocale", "locale"]

parseQuotingStyle :: T.Text -> Maybe QuotingStyle
parseQuotingStyle = \case
  "literal" -> Just Literal
  "shell" -> Just Shell
  "shell-always" -> Just ShellAlways
  "shell-escape" -> Just ShellEscape
  "shell-escape-always" -> Just ShellEscapeAlways
  "c" -> Just C
  "escape" -> Just Escape
  "clocale" -> Just $ CLocale '`' '\''
  "locale" -> Just $ Locale '`' '\''
  _ -> Nothing
