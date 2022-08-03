-- | This is a base module to construst 'LS_COLORS' or similar extensions.
module Haskellorls.LsColor.Config
  ( wrap,
    Options (..),
    Sources (..),
    module Haskellorls.LsColor.Class,
  )
where

import Control.Applicative
import Data.Default.Class
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Haskellorls.Class
import Haskellorls.LsColor.Class
import Prelude hiding (lookup)

-- | A dictionary, which contains raw parsed results of 'LS_COLORS' or similar
-- extensions.
newtype Sources = Sources {unSources :: M.Map T.Text T.Text}

instance Dictionary T.Text T.Text Sources where
  lookup t (Sources m) = t `lookup` m

instance From T.Text Sources where
  from = Sources . M.fromList . mapMaybe (makePair . T.splitOn "=") . T.splitOn ":"
    where
      makePair :: [T.Text] -> Maybe (T.Text, T.Text)
      makePair [k, v] = Just (k, v)
      makePair _ = Nothing

instance Deserialize Sources

-- | An base data type to construct 'LS_COLORS' or similar extensions. This
-- contains all constants in 'man 5 dir_colors' and has an extra field for an
-- extension. Even if 'LS_COLORS', we should implement it by using the
-- extension field.
data Options a e = Options
  { left :: Maybe a,
    right :: Maybe a,
    end :: Maybe a,
    reset :: Maybe a,
    normal :: Maybe a,
    file :: Maybe a,
    directory :: Maybe a,
    symlink :: Maybe a,
    pipe :: Maybe a,
    socket :: Maybe a,
    block :: Maybe a,
    char :: Maybe a,
    missing :: Maybe a,
    orphan :: Maybe a,
    executable :: Maybe a,
    door :: Maybe a,
    setuid :: Maybe a,
    setgid :: Maybe a,
    sticky :: Maybe a,
    otherWritable :: Maybe a,
    stickyOtherWritable :: Maybe a,
    capability :: Maybe a,
    multiHardlink :: Maybe a,
    clearLine :: Maybe a,
    extension :: Maybe e
  }
  deriving (Show)

instance (Deserialize a, From Sources e, Default (Options a e)) => From T.Text (Options a e) where
  from = from @Sources . from

instance (Deserialize a, From Sources e, Default (Options a e)) => Deserialize (Options a e)

instance (Deserialize a, From Sources e, Default (Options a e)) => From Sources (Options a e) where
  from s =
    Options
      { left = deserialize <$> "lc" `lookup'` s <|> left,
        right = deserialize <$> "rc" `lookup'` s <|> right,
        end = deserialize <$> "ec" `lookup'` s <|> end,
        reset = deserialize <$> "rs" `lookup'` s <|> reset,
        normal = deserialize <$> "no" `lookup'` s <|> normal,
        file = deserialize <$> "fi" `lookup'` s <|> file,
        directory = deserialize <$> "di" `lookup'` s <|> directory,
        symlink = deserialize <$> "ln" `lookup'` s <|> symlink,
        pipe = deserialize <$> "pi" `lookup'` s <|> pipe,
        socket = deserialize <$> "so" `lookup'` s <|> socket,
        block = deserialize <$> "bd" `lookup'` s <|> block,
        char = deserialize <$> "cd" `lookup'` s <|> char,
        missing = deserialize <$> "mi" `lookup'` s <|> missing,
        orphan = deserialize <$> "or" `lookup'` s <|> orphan,
        executable = deserialize <$> "ex" `lookup'` s <|> executable,
        door = deserialize <$> "do" `lookup'` s <|> door,
        setuid = deserialize <$> "su" `lookup'` s <|> setuid,
        setgid = deserialize <$> "sg" `lookup'` s <|> setgid,
        sticky = deserialize <$> "st" `lookup'` s <|> sticky,
        otherWritable = deserialize <$> "ow" `lookup'` s <|> otherWritable,
        stickyOtherWritable = deserialize <$> "tw" `lookup'` s <|> stickyOtherWritable,
        capability = deserialize <$> "ca" `lookup'` s <|> capability,
        multiHardlink = deserialize <$> "mh" `lookup'` s <|> multiHardlink,
        clearLine = deserialize <$> "cl" `lookup'` s <|> clearLine,
        extension = Just (from s) <|> extension
      }
    where
      Options {..} = def
      lookup' :: T.Text -> Sources -> Maybe T.Text
      lookup' = lookup

-- | Wrap an escape sequence parameter with 'left' aka 'LEFTCODE' and 'right'
-- aka 'RIGHTCODE'. Expected outputs are such as '\033[0m' and '\e[38;5;81m'.
wrap :: (Semigroup a) => Options a e -> a -> a
wrap (Options {..}) a = case (left, right) of
  (Just l, Just r) -> l <> a <> r
  _ -> a
