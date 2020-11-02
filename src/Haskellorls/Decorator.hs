module Haskellorls.Decorator where

import Data.List (transpose)

import Haskellorls.Node

decorator :: [Node] -> [Node -> String] -> [String]
decorator nodes = map join . transpose . map (\f -> decorator' nodes f)
  where join = foldr1 (\l r -> l ++ " " ++ r)

decorator' :: [Node] -> (Node -> String) -> [String]
decorator' nodes f = map (leftPadding ' ' maxLength) nodes'
  where nodes' = map f nodes
        maxLength = maximum . map length $ nodes'

leftPadding :: Char -> Int -> String -> String
leftPadding c n s | n > len = pad ++ s
  where len = length s
        padSize = n - len
        pad = take padSize . repeat $ c
leftPadding _ _ s = s
