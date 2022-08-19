module Haskellorls.Formatter.WrappedText
  ( WrappedText (..),
    modify,
    wrap,
    justifyLeft,
    justifyRight,
    module Haskellorls.Class,
  )
where

import qualified Data.Text as T
import Haskellorls.Class
import qualified Haskellorls.LsColor.Config as Config

-- | A 'Text', which are surrounded with two 'Text's. We assume that these
-- surround texts are invisible on a terminal, these usually are valid SGR
-- sequences, so we should treat this value as just a 'Text' of 'wtWord' when
-- determine the output layout, and should surround it with 'wtPreffix' and
-- 'wtSuffix' when render this value.
--
-- No need to evaluate 'wtPreffix' and 'wtSuffix' until render it.
--
-- FIXME: Needless to say, this value stores thunks. We maybe need to change
-- this application's architecture not to depend on these lazy evaluations.
data WrappedText = WrappedText
  { wtPrefix :: ~T.Text,
    wtWord :: T.Text,
    wtSuffix :: ~T.Text
  }

instance From WrappedText T.Text where
  from (WrappedText {..}) = wtPrefix <> wtWord <> wtSuffix

instance From T.Text WrappedText where
  from t = WrappedText "" t ""

instance TerminalLength WrappedText where
  termLength = termLength . wtWord

-- | Modify only 'wtWord' in 'WrappedText'.
modify :: (T.Text -> T.Text) -> WrappedText -> WrappedText
modify f WrappedText {..} = WrappedText wtPrefix (f wtWord) wtSuffix

-- | A wrapper utility to generate wrapped text using 'LS_COLORS' or similar
-- extension configurations.
wrap :: (Semigroup a, From a T.Text) => Config.Options a e -> (Config.Options a e -> Maybe a) -> T.Text -> WrappedText
wrap o@(Config.Options {..}) getter t = case getter o of
  Just esc -> WrappedText (from $ Config.wrap o esc) t $ maybe "" (from . Config.wrap o) reset
  _ -> from t

-- | Left-justify a list of 'WrappedText' to the given length, using the given
-- character.
justifyLeft :: Int -> Char -> [WrappedText] -> [WrappedText]
justifyLeft n c wt
  | diff <- n - l, diff > 0 = wt <> [from (T.replicate diff $ T.singleton c)]
  | otherwise = wt
  where
    l = sum $ termLength <$> wt

-- | Right-justify a list of 'WrappedText' to the given length, using the given
-- character.
justifyRight :: Int -> Char -> [WrappedText] -> [WrappedText]
justifyRight n c wt
  | diff <- n - l, diff > 0 = from (T.replicate diff $ T.singleton c) : wt
  | otherwise = wt
  where
    l = sum $ termLength <$> wt
