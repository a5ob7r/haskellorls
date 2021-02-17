module Haskellorls.Depth
  ( Depth,
    makeDepth,
    makeInf,
    isDepthZero,
    decreaseDepth,
  )
where

data Depth
  = Depth Int
  | INF

instance Eq Depth where
  INF == INF = True
  Depth a == Depth b = a == b
  _ == _ = False

makeDepth :: Int -> Maybe Depth
makeDepth n
  | n > -1 = Just $ Depth n
  | otherwise = Nothing

makeInf :: Depth
makeInf = INF

isDepthZero :: Depth -> Bool
isDepthZero d = d == Depth 0

decreaseDepth :: Depth -> Depth
decreaseDepth d = case d of
  INF -> INF
  Depth n
    | n > 0 -> Depth (n - 1)
    | otherwise -> Depth 0
