module Haskellorls.Tree (
  Depth,
  makeDepth,
  makeInf,
  getDepth,
  decreaseDepth,
) where

data Depth
  = Depth Int
  | INF

makeDepth :: Int -> Maybe Depth
makeDepth n
  | n > -1 = Just $ Depth n
  | otherwise = Nothing

makeInf :: Depth
makeInf = INF

getDepth :: Depth -> Maybe Int
getDepth d = case d of
  INF -> Nothing
  Depth n -> Just n

decreaseDepth :: Depth -> Depth
decreaseDepth d = case d of
  INF -> INF
  Depth n
    | n > 0 -> Depth (n - 1)
    | otherwise -> Depth 0
