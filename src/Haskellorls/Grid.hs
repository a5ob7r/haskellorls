module Haskellorls.Grid
  ( showNodesWithGridForm
  ) where

import Data.List (intercalate, transpose)
import qualified System.Console.Terminal.Size as TS

import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Color as Color

newtype Grid = Grid { unGrid :: [[String]] }
  deriving Show

showNodesWithGridForm :: [Node.NodeInfo] -> (Node.NodeInfo -> String) -> IO ()
showNodesWithGridForm nodes nodePrinter = do
  colSize <- terminalColumnSize
  let names = map Color.nodeName nodes
      grid = validGrid names colSize
      maxColLens = maximumColumnLengths grid
      nodeGrid = transNest (length $ unGrid grid) nodes
      gridLines = map (\nds -> gridLine nds maxColLens nodePrinter) nodeGrid
  mapM_ putStrLn gridLines

gridMargin :: String
gridMargin = "  "

validateGrid :: Int -> Grid -> Bool
validateGrid n grid
  | n == 0 = False
  | otherwise = n >= sum maxColLens + paddings
    where
      maxColLens = maximumColumnLengths grid
      paddings = length gridMargin * relu (length maxColLens - 1)

relu :: Int -> Int
relu n
  | n >= 0 = n
  | otherwise = -n

gridLine :: [Node.NodeInfo] -> [Int] -> (Node.NodeInfo -> String) -> String
gridLine ns maxColLens nodePrinter = intercalate gridMargin $ zipWith (\nd pad -> nodePrinter nd ++ replicate pad ' ') ns paddings
  where
    paddings = zipWith (\nd colLen -> colLen - length (Color.nodeName nd)) ns maxColLens

validGrid :: [String] -> Int -> Grid
validGrid ss colSize = validGrid' ss colSize 1

validGrid' :: [String] -> Int -> Int -> Grid
validGrid' ss colSize colNum
  | ssNum <= colNum = grid
  | validateGrid colSize grid = grid
  | otherwise = validGrid' ss colSize $ colNum + 1
    where
      grid = makeGrid colNum ss
      ssNum = length ss

maximumColumnLengths :: Grid -> [Int]
maximumColumnLengths (Grid grid) = map (maximum . map length) . transpose $ grid

terminalColumnSize :: IO Int
terminalColumnSize = maybe 0 TS.width <$> TS.size

makeGrid :: Int -> [String] -> Grid
makeGrid n = Grid . transNest n

transNest :: Int -> [a] -> [[a]]
transNest n = transpose . nest n

nest :: Int -> [a] -> [[a]]
nest n xs = if length xs > n
               then take n xs:nest n (drop n xs)
               else [xs]
