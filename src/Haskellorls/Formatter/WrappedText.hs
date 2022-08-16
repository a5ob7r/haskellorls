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

data WrappedText = WrappedText
  { wtPrefix :: T.Text,
    wtWord :: T.Text,
    wtSuffix :: T.Text
  }

instance From WrappedText T.Text where
  from (WrappedText {..}) = wtPrefix <> wtWord <> wtSuffix

instance Serialize WrappedText

instance From T.Text WrappedText where
  from t = WrappedText "" t ""

instance Deserialize WrappedText

instance TerminalLength WrappedText where
  termLength = termLength . wtWord

-- | Modify only 'wtWord' in 'WrappedText'.
modify :: (T.Text -> T.Text) -> WrappedText -> WrappedText
modify f WrappedText {..} = WrappedText wtPrefix (f wtWord) wtSuffix

-- | A wrapper utility to generate wrapped text using 'LS_COLORS' or similar
-- extension configurations.
wrap :: (Semigroup a, Serialize a) => Config.Options a e -> (Config.Options a e -> Maybe a) -> T.Text -> WrappedText
wrap o@(Config.Options {..}) getter t = case getter o of
  Just esc -> WrappedText (serialize $ Config.wrap o esc) t $ maybe "" (serialize . Config.wrap o) reset
  _ -> WrappedText "" t ""

-- | Left-justify a list of 'WrappedText' to the given length, using the given
-- character.
justifyLeft :: Int -> Char -> [WrappedText] -> [WrappedText]
justifyLeft n c wt
  | diff <- n - l, diff > 0 = wt <> [deserialize (T.replicate diff $ T.singleton c)]
  | otherwise = wt
  where
    l = sum $ termLength <$> wt

-- | Right-justify a list of 'WrappedText' to the given length, using the given
-- character.
justifyRight :: Int -> Char -> [WrappedText] -> [WrappedText]
justifyRight n c wt
  | diff <- n - l, diff > 0 = deserialize (T.replicate diff $ T.singleton c) : wt
  | otherwise = wt
  where
    l = sum $ termLength <$> wt
