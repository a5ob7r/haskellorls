module Haskellorls.Class
  ( Dictionary (..),
    TerminalLength (..),
    Notifiable (..),
  )
where

import Control.Exception (IOException, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isLatin1)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import System.IO (hPrint, stderr)

class Dictionary k v d where
  lookup :: k -> d -> Maybe v

instance (Ord k) => Dictionary k v (M.Map k v) where
  lookup k d = k `M.lookup` d

-- | A instance of "TerminalLength" has a length to dipslay it on a terminal.
class TerminalLength a where
  -- | When display @a@ on a terminal, it consumes columns of @termLength a@.
  termLength :: a -> Int

instance TerminalLength Char where
  -- FIXME: We assume non-Latin1 charactor has double in width of any Latin1
  -- charactor to display it on a terminal. However this is a very naive
  -- approach, a lot of icon glyphs in @Nerd Fonts@ doesn't match this
  -- assumption. For example, maybe we should detect whether or not the
  -- character is one of CJK.
  --
  -- [@Nerd Fonts@]: https://www.nerdfonts.com/
  termLength c = if isLatin1 c then 1 else 2

instance TerminalLength T.Text where
  termLength = T.foldl' (\acc c -> acc + termLength c) 0

-- | Notify a information to users by printing a message to stderr. This is
-- similar to a shell command @echo message >&2@.
class (Show a) => Notifiable a where
  notify :: (MonadIO m) => a -> m ()
  notify = liftIO . hPrint stderr

instance Notifiable IOException

instance Notifiable SomeException
