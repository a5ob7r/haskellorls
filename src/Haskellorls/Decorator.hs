module Haskellorls.Decorator
  ( decorator
  ) where

import Data.List (transpose)

import Haskellorls.NodeInfo
import qualified Haskellorls.YetAnotherString as YAString (YetAnotherString (..), WrapedString)

decorator :: [NodeInfo] -> [NodeInfo -> [YAString.WrapedString]] -> [String]
decorator nodes = map join . transpose . map (decorator' nodes)
  where join = foldr1 (\l r -> l ++ " " ++ r)

decorator' :: [NodeInfo] -> (NodeInfo -> [YAString.WrapedString]) -> [String]
decorator' nodes f = map (leftPadding ' ' maxLength) nodes'
  where nodes' = map f nodes
        maxLength = maximum . map YAString.yaLength $ nodes'

leftPadding :: YAString.YetAnotherString yaStr => Char -> Int -> yaStr -> String
leftPadding c n yaStr | n > len = pad ++ YAString.yaShow' yaStr
  where len = YAString.yaLength yaStr
        padSize = n - len
        pad = replicate padSize c
leftPadding _ _ yaStr = YAString.yaShow' yaStr
