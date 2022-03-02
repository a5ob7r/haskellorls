module Haskellorls.Quote.Utils
  ( quote,
    quoteStyle,
    quoteStyleForLink,
    charactorsNeedQuote,
    lookupQuotingStyle,
    module Haskellorls.Quote.Type,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Haskellorls.Option as Option
import Haskellorls.Quote.Option
import Haskellorls.Quote.Type
import qualified Haskellorls.WrappedText as WT
import qualified System.Environment as Env

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
quote :: QuoteStyle -> WT.WrappedText -> [WT.WrappedText]
quote style wt@WT.WrappedText {..} = case style of
  NoQuote -> [wt]
  SpacePadding -> [WT.toWrappedText " ", wt]
  SingleQuote -> [wt {WT.wtWord = "'" <> wtWord <> "'"}]
  DoubleQuote -> [wt {WT.wtWord = "\"" <> escapeDoubleQuote wtWord <> "\""}]
  _
    | '\'' `Set.member` setA -> quote DoubleQuote wt
    | Set.size (Set.intersection setA setB) > 0 -> quote SingleQuote wt
    | otherwise -> case style of
        DynamicQuote -> quote SpacePadding wt
        _ -> quote NoQuote wt
    where
      setA = textToSet wtWord
      setB = Set.fromList charactorsNeedQuote

-- WIP: Should implement singole quote version. Also need to change printer
-- architecture.
quoteStyle :: Option.Option -> QuoteStyle
quoteStyle opt = case Option.quotingStyle opt of
  _
    | Option.literal opt -> NoQuote
    | Option.quoteName opt -> DoubleQuote
  Literal -> NoQuote
  Shell -> DynamicQuote
  ShellAlways -> SingleQuote
  ShellEscape -> DynamicQuote
  ShellEscapeAlways -> SingleQuote
  C -> DoubleQuote
  Escape -> NoQuote
  _
    | Option.noQuote opt -> NoQuote
    | otherwise -> DynamicQuote

quoteStyleForLink :: Option.Option -> QuoteStyle
quoteStyleForLink opt = case style of
  DynamicQuote -> DynamicQuoteForLink
  _ -> style
  where
    style = quoteStyle opt

textToSet :: T.Text -> Set.Set Char
textToSet = Set.fromList . T.unpack

charactorsNeedQuote :: String
charactorsNeedQuote = " !\"$&()*;<=>[^`|"

escapeDoubleQuote :: T.Text -> T.Text
escapeDoubleQuote = T.concatMap $ \case
  '"' -> "\\\""
  c -> T.singleton c

lookupQuotingStyle :: Option.Option -> IO QuotingStyle
lookupQuotingStyle opt = case Option.quotingStyle opt of
  NoStyle -> do
    Env.lookupEnv "QUOTING_STYLE" >>= \case
      Just styleFromEnv ->
        case parseQuotingStyle (T.pack styleFromEnv) of
          Nothing -> pure NoStyle
          Just style -> pure style
      Nothing -> pure NoStyle
  style -> pure style
