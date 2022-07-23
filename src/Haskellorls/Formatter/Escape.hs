module Haskellorls.Formatter.Escape (escapeFormatter) where

import Data.Char
import qualified Data.Text as T
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Quote as Quote

replaceControlCharsToQuestion :: T.Text -> T.Text
replaceControlCharsToQuestion = T.map (\c -> if isPrint c then c else '?')

escapeCharsForStdout :: T.Text -> T.Text
escapeCharsForStdout = T.concatMap $ \case
  '\r' -> "'$'\\r''"
  '\t' -> "'$'\\t''"
  c -> T.singleton c

-- | NOTE: There may be some missing targets.
escapeCharsForStdoutByCStyle :: T.Text -> T.Text
escapeCharsForStdoutByCStyle = T.concatMap $ \case
  '\t' -> "\\t"
  '\r' -> "\\r"
  '\n' -> "\\n"
  ' ' -> "\\ "
  '|' -> "\\|"
  '\\' -> "\\\\"
  c -> T.singleton c

escapeFormatter :: Config.Config -> T.Text -> T.Text
escapeFormatter config = case Config.quotingStyle config of
  Quote.Literal -> replaceControlCharsToQuestion
  Quote.Shell -> replaceControlCharsToQuestion
  Quote.ShellAlways -> replaceControlCharsToQuestion
  Quote.ShellEscape -> escapeCharsForStdout
  Quote.ShellEscapeAlways -> escapeCharsForStdout
  Quote.C -> escapeCharsForStdoutByCStyle
  Quote.Escape -> escapeCharsForStdoutByCStyle
  Quote.NoStyle
    | Config.toTTY config -> escapeCharsForStdout
    | otherwise -> id
