module Haskellorls.Config.Depth
  ( Depth,
    makeDepth,
    makeInf,
    makeZero,
    isDepthZero,
    increaseDepth,
    decreaseDepth,
  )
where

data InfValue a = Value a | INF
  deriving (Eq, Ord, Functor)

type Depth = InfValue Int

makeDepth :: Int -> Maybe Depth
makeDepth n
  | n > -1 = Just $ Value n
  | otherwise = Nothing

makeInf :: Depth
makeInf = INF

makeZero :: Depth
makeZero = Value 0

isDepthZero :: Depth -> Bool
isDepthZero d = d == Value 0

increaseDepth :: Depth -> Depth
increaseDepth = fmap (+ 1)

decreaseDepth :: Depth -> Depth
decreaseDepth = \case
  INF -> INF
  Value n
    | n > 0 -> Value (n - 1)
    | otherwise -> Value 0
