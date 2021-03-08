module Haskellorls.Quote.Type
  ( QuoteStyle (..),
    QuotingStyle (..),
  )
where

data QuoteStyle
  = NoQuote
  | SpacePadding
  | SingleQuote
  | DoubleQuote
  | DynamicQuote
  | DynamicQuoteForLink

-- | For option parsing.
--
-- NoStyle:
--   no specified
-- Literal:
--   same to '--literal'
-- Shell:
--   quote according to default rule and replace control characters to '?'
-- ShellAlways:
--   force quote and replace control characters to '?'
-- ShellEscape:
--   default
-- ShellEscapeAlways:
--   force quote and default
-- C:
--   double quote and '--escape'
-- Escape:
--   same to '--escape'
--
-- WIP: Add 'locale'.
data QuotingStyle
  = NoStyle
  | Literal
  | Shell
  | ShellAlways
  | ShellEscape
  | ShellEscapeAlways
  | C
  | Escape
