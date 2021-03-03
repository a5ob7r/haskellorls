module Haskellorls.Quote.Type
  ( QuoteStyle (..),
  )
where

data QuoteStyle
  = NoQuote
  | SpacePadding
  | SingleQuote
  | DoubleQuote
  | DynamicQuote
  | DynamicQuoteForLink
