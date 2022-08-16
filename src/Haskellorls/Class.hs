module Haskellorls.Class
  ( Deserialize (..),
    Dictionary (..),
    From (..),
    Serialize (..),
    TerminalLength (..),
  )
where

import Data.Char (isLatin1)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

class Dictionary k v d where
  lookup :: k -> d -> Maybe v

instance (Ord k) => Dictionary k v (M.Map k v) where
  lookup k d = k `M.lookup` d

-- | Generaete 'b' type value from 'a' type value.
class From a b where
  from :: a -> b

-- | Specialized version of 'From a Text'.
class (From a T.Text) => Serialize a where
  serialize :: a -> T.Text
  serialize = from

-- | Specialized version of 'From Text a'.
class (From T.Text a) => Deserialize a where
  deserialize :: T.Text -> a
  deserialize = from

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
