module Haskellorls.Formatter.Quote
  ( quote,
    quoteStyle,
    quoteStyleForLink,
    charactorsNeedQuote,
    module Haskellorls.Config.Quote,
  )
where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Option.Quote
import Haskellorls.Config.Quote
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT

-- filename:
--   - double quote (-Q / --quote-name)
--   - no quote (-N / --literal)
--   - dynamic quote:
--     - no quote (when no filename which need to quote)
--     - pad space to head (when there are filename which need to quote)
--     - single quote (when a filename have some charactors which are need to quote)
--     - double quote (when a filename have some single quotes)
-- linkname:
--   - double quote (-Q / --quote-name)
--   - no quote (-N / --literal)
--   - dynamic quote:
--     - no quote (when no filename which need to quote)
--     - single quote (when a filename have some charactors which are need to quote)
--     - double quote (when a filename have some single quotes)
quote :: QuoteStyle -> Attr.Attribute WT.WrappedText -> [Attr.Attribute WT.WrappedText]
quote style wt = case style of
  NoQuote -> [wt]
  SpacePadding -> [Attr.Other $ WT.deserialize " ", wt]
  SingleQuote -> [WT.modify (\t -> "'" <> t <> "'") <$> wt]
  DoubleQuote -> [WT.modify (\t -> "\"" <> escapeDoubleQuote t <> "\"") <$> wt]
  _
    | '\'' `S.member` setA -> quote DoubleQuote wt
    | not $ S.disjoint setA setB -> quote SingleQuote wt
    | otherwise -> case style of
        DynamicQuote -> quote SpacePadding wt
        _ -> quote NoQuote wt
    where
      t = WT.wtWord $ Attr.unwrap wt
      setA = T.foldl' (flip S.insert) mempty t
      setB = S.fromList charactorsNeedQuote

-- WIP: Should implement singole quote version. Also need to change printer
-- architecture.
quoteStyle :: Config.Config -> QuoteStyle
quoteStyle config = case Config.quotingStyle config of
  _ | Config.quoteName config -> DoubleQuote
  Literal -> NoQuote
  Shell -> DynamicQuote
  ShellAlways -> SingleQuote
  ShellEscape -> DynamicQuote
  ShellEscapeAlways -> SingleQuote
  C -> DoubleQuote
  Escape -> NoQuote
  _
    | Config.noQuote config -> NoQuote
    | otherwise -> DynamicQuote

quoteStyleForLink :: Config.Config -> QuoteStyle
quoteStyleForLink config = case style of
  DynamicQuote -> DynamicQuoteForLink
  _ -> style
  where
    style = quoteStyle config

charactorsNeedQuote :: String
charactorsNeedQuote = " !\"$&()*;<=>[^`|"

escapeDoubleQuote :: T.Text -> T.Text
escapeDoubleQuote = T.concatMap $ \case
  '"' -> "\\\""
  c -> T.singleton c
