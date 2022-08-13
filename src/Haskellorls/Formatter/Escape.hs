module Haskellorls.Formatter.Escape (escape) where

import Data.Char
import qualified Data.Text as T
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Quote as Quote

replaceControlCharsByQuestion :: T.Text -> T.Text
replaceControlCharsByQuestion = T.map (\c -> if isPrint c then c else '?')

escapeCharsForStdout :: T.Text -> T.Text
escapeCharsForStdout = T.concatMap $ \case
  '\r' -> "'$'\\r''"
  '\t' -> "'$'\\t''"
  c -> T.singleton c

-- | The @'escapeAsCLiteral' b t@ function substitutes characters in 'Text'
-- with C string literal expression. @b@ indicates whether or not the
-- substituted string will be surrounded by double quotes. If it is 'True' this
-- function also substitutes @"@ with an appropriate expression in addition to
-- standard targets, otherwise also substitutes @ @ (a space) with an
-- appropriate one too.
--
-- NOTE: Should we substitute other non-printable characters? And can these
-- characters be contained in a valid filepath?
escapeAsCLiteral :: Bool -> T.Text -> T.Text
escapeAsCLiteral willQuote = T.concatMap $ \case
  '"' | willQuote -> "\""
  ' ' | not willQuote -> "\\ "
  '\t' -> "\\t"
  '\r' -> "\\r"
  '\n' -> "\\n"
  '\\' -> "\\\\"
  c -> T.singleton c

escape :: Config.Config -> T.Text -> T.Text
escape config = case Config.quotingStyle config of
  Quote.Literal
    | Config.showControlChars config -> id
    | otherwise -> replaceControlCharsByQuestion
  Quote.Shell -> replaceControlCharsByQuestion
  Quote.ShellAlways -> replaceControlCharsByQuestion
  Quote.ShellEscape -> escapeCharsForStdout
  Quote.ShellEscapeAlways -> escapeCharsForStdout
  Quote.C -> escapeAsCLiteral True
  Quote.Escape -> escapeAsCLiteral False
