module Haskellorls.Formatter.Quote
  ( quote,
    module Haskellorls.Config.Quote,
  )
where

import Data.Char (isPrint)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Quote
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT

-- | Quote and Escape for filenames.
quote :: Config.Config -> Attr.Attribute WT.WrappedText -> Attr.Attribute WT.WrappedText
quote config wt = case Config.quotingStyle config of
  Literal
    | Config.showControlChars config -> wt
    | otherwise -> WT.modify replaceControlCharsByQuestion <$> wt
  Shell -> quoteForShell config False wt
  ShellAlways -> quoteForShell config True wt
  ShellEscape -> quoteForShellEscape False wt
  ShellEscapeAlways -> quoteForShellEscape True wt
  C -> WT.modify (\t -> "\"" <> escapeAsCLiteral "\"" t <> "\"") <$> wt
  Escape -> WT.modify (escapeAsCLiteral " ") <$> wt
  CLocale l r -> WT.modify (\t -> T.singleton l <> escapeAsCLiteral [r] t <> T.singleton r) <$> wt
  Locale l r -> WT.modify (\t -> T.singleton l <> escapeAsCLiteral [r] t <> T.singleton r) <$> wt

quoteForShell :: Config.Config -> Bool -> Attr.Attribute WT.WrappedText -> Attr.Attribute WT.WrappedText
quoteForShell config forceQuote wt
  | not forceQuote && S.disjoint chars charactorsNeedQuote = WT.modify (const t) <$> wt
  | '\'' `S.notMember` chars = WT.modify (const $ "'" <> t <> "'") <$> wt
  | '"' `S.notMember` chars = WT.modify (const $ "\"" <> t <> "\"") <$> wt
  | otherwise = WT.modify (const $ "'" <> T.concatMap (\c -> if c == '\'' then "'\\''" else T.singleton c) t <> "'") <$> wt
  where
    escape
      | Config.showControlChars config = id
      | otherwise = replaceControlCharsByQuestion
    t = escape . WT.wtWord $ Attr.unwrap wt
    chars = T.foldl' (flip S.insert) mempty t

quoteForShellEscape :: Bool -> Attr.Attribute WT.WrappedText -> Attr.Attribute WT.WrappedText
quoteForShellEscape forceQuote wt
  | not forceQuote && S.disjoint chars charactorsNeedQuote && isAllCharsPrintable = wt
  | '\'' `S.notMember` chars = WT.modify (const $ "'" <> escape t <> "'") <$> wt
  | '"' `S.notMember` chars && isAllCharsPrintable = WT.modify (const $ "\"" <> t <> "\"") <$> wt
  | otherwise = WT.modify (const $ "'" <> escape t <> "'") <$> wt
  where
    t = WT.wtWord $ Attr.unwrap wt
    chars = T.foldl' (flip S.insert) mempty t
    isAllCharsPrintable = T.all isPrint t
    escape = T.concatMap $ \case
      '\r' -> "'$'\\r''"
      '\n' -> "'$'\\n''"
      '\t' -> "'$'\\t''"
      '\'' -> "'\''"
      c -> T.singleton c

-- | The @'escapeAsCLiteral' cs t@ function substitutes characters in 'Text'
-- with C string literal expression. In addition to them, characters in @cs@
-- are escaped too.
--
-- NOTE: Should we substitute other non-printable characters? And can these
-- characters be contained in a valid filepath?
escapeAsCLiteral :: String -> T.Text -> T.Text
escapeAsCLiteral chars = T.concatMap $ \case
  '\t' -> "\\t"
  '\r' -> "\\r"
  '\n' -> "\\n"
  '\\' -> "\\\\"
  c
    | c `elem` chars -> '\\' `T.cons` T.singleton c
    | otherwise -> T.singleton c

replaceControlCharsByQuestion :: T.Text -> T.Text
replaceControlCharsByQuestion = T.map (\c -> if isPrint c then c else '?')

-- | These characters should be quoted for shell.
charactorsNeedQuote :: S.Set Char
charactorsNeedQuote = S.fromList " !\"'$&()*;<=>[^`|\\"
