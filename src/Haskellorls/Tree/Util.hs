{-# LANGUAGE MultiWayIf #-}

module Haskellorls.Tree.Util
  ( makeSomeNewPositionsList,
    makeTreeNodeInfos,
  )
where

import qualified Control.Monad.Extra as M
import qualified Data.List.Extra as L
import qualified Data.Sequence as S
import qualified Haskellorls.Depth as Depth
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Sort.Method as Sort
import Haskellorls.Tree.Type
import qualified Haskellorls.Utils as Utils
import qualified System.FilePath.Posix as Posix
import qualified System.Posix.Files as Files

makeSomeNewPositionsList :: Int -> [TreeNodePosition] -> [[TreeNodePosition]]
makeSomeNewPositionsList n _
  | n < 1 = [[]]
makeSomeNewPositionsList 1 xs = [L.snoc xs LAST]
makeSomeNewPositionsList n xs = case replicate n xs of
  [] -> []
  (y : ys) -> L.snoc y HEAD : makeSomeNewPositionsList' ys

makeSomeNewPositionsList' :: [[TreeNodePosition]] -> [[TreeNodePosition]]
makeSomeNewPositionsList' [] = []
makeSomeNewPositionsList' [x] = [L.snoc x LAST]
makeSomeNewPositionsList' (x : xs) = L.snoc x MID : makeSomeNewPositionsList' xs

makeTreeNodeInfos :: Option.Option -> FilePath -> IO (S.Seq Node.NodeInfo)
makeTreeNodeInfos opt path = do
  node <- Node.nodeInfo opt "" path
  makeTreeNodeInfos' opt path node

makeTreeNodeInfos' :: Option.Option -> FilePath -> Node.NodeInfo -> IO (S.Seq Node.NodeInfo)
makeTreeNodeInfos' opt path node = do
  contents <-
    if
        | Depth.isDepthZero depth || not (Files.isDirectory $ Node.nodeInfoStatus node) -> pure []
        -- Force hide '.' and '..' to avoid infinite loop.
        | Option.all opt -> Utils.listContents opt {Option.all = False, Option.almostAll = True} path
        | otherwise -> Utils.listContents opt path
  let pList = makeSomeNewPositionsList (length contents) $ Node.getTreeNodePositions node
  nodes <- Sort.sorter opt <$> mapM (Node.nodeInfo opt path) contents
  let nodes' = zipWith (\nd p -> nd {Node.getTreeNodePositions = p}) nodes pList
  (node S.<|) <$> M.mconcatMapM (\nd -> makeTreeNodeInfos' opt' (path Posix.</> Node.nodeInfoPath nd) nd) nodes'
  where
    opt' = opt {Option.level = Depth.decreaseDepth depth}
    depth = Option.level opt