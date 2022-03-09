module Haskellorls.Class
  ( Deserialize (..),
    Dictionary (..),
    From (..),
    Serialize (..),
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T

class Dictionary k v d where
  lookup :: k -> d -> Maybe v

instance Dictionary Int a [a] where
  lookup = flip (!!?)

instance (Ord k) => Dictionary k v (M.Map k v) where
  lookup k d = k `M.lookup` d

nth :: [a] -> Int -> Maybe a
nth [] _ = Nothing
nth _ n | n < 0 = Nothing
nth (x : _) 0 = Just x
nth (_ : xs) n = nth xs (n - 1)

(!!?) :: [a] -> Int -> Maybe a
(!!?) = nth

-- | Generaete 'b' type value from 'a' type value.
class From a b where
  from :: a -> b

instance From a a where
  from = id

instance (Monoid a) => From (Maybe a) a where
  from = fromMaybe mempty

-- | Specialized version of 'From a Text'.
class (From a T.Text) => Serialize a where
  serialize :: a -> T.Text
  serialize = from

-- | Specialized version of 'From Text a'.
class (From T.Text a) => Deserialize a where
  deserialize :: T.Text -> a
  deserialize = from
