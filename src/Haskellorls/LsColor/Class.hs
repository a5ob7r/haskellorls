module Haskellorls.LsColor.Class
  ( Query (..),
    Queryable (..),
  )
where

import Data.Maybe
import Data.String
import qualified Data.Text as T
import Haskellorls.Class
import Prelude hiding (lookup)

-- | Query using filename.
class (Dictionary Query v d) => Queryable v d where
  -- This default implementation matches shortest suffix pattern.
  query :: Query -> d -> Maybe v
  query q d = listToMaybe . mapMaybe (`lookup` d) $ queries q

-- | A query parameter.
newtype Query = Query {unQuery :: T.Text}
  deriving (Eq, Ord, Show, IsString)

-- | A 'tails' wrapper for 'Query'.
queries :: Query -> [Query]
queries = map (Query . T.toUpper) . reverse . T.tails . unQuery
