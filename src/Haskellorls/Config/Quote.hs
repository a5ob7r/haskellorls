module Haskellorls.Config.Quote (QuotingStyle (..)) where

data QuotingStyle
  = Literal
  | Shell
  | ShellAlways
  | ShellEscape
  | ShellEscapeAlways
  | C
  | Escape
  | CLocale Char Char
  | Locale Char Char
