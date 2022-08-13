module Haskellorls.Config.Quote
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
-- 'Literal':
--   same to @--literal@
-- 'Shell':
--   quote according to default rule and replace control characters to '?'
-- 'ShellAlways':
--   force quote and replace control characters to '?'
-- 'ShellEscape':
--   default
-- 'ShellEscapeAlways':
--   force quote and default
-- 'C':
--   double quote and @--escape@
-- 'Escape':
--   same to @--escape@
--
-- WIP: Add 'Locale' for @locale@
data QuotingStyle
  = Literal
  | Shell
  | ShellAlways
  | ShellEscape
  | ShellEscapeAlways
  | C
  | Escape
