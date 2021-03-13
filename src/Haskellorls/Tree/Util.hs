{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Haskellorls.Tree.Util
  ( makeSomeNewPositionsList,
    makeTreeNodeInfos,
  )
where

import qualified Data.List.Extra as L
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Haskellorls.Depth as Depth
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Recursive.Utils as Recursive
import qualified Haskellorls.Sort.Method as Sort
import Haskellorls.Tree.Type
import qualified Haskellorls.Utils as Utils
import qualified System.FilePath.Posix as Posix
import qualified System.IO as IO

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
  let inodeSet = Recursive.InodeSet . Set.singleton . Node.pfsFileID $ Node.nodeInfoStatus node
  makeTreeNodeInfos' inodeSet opt $ S.singleton node

-- | With error message output.
makeTreeNodeInfos' :: Recursive.InodeSet -> Option.Option -> S.Seq Node.NodeInfo -> IO (S.Seq Node.NodeInfo)
makeTreeNodeInfos' _ _ S.Empty = pure S.empty
makeTreeNodeInfos' inodeSet opt (node S.:<| nodeSeq) = do
  let path = Node.nodeInfoDirName node Posix.</> Node.nodeInfoPath node
  contents <-
    if
        | Depth.isDepthZero depth || not (Node.isDirectory . Node.pfsNodeType $ Node.nodeInfoStatus node) -> pure []
        -- Force hide '.' and '..' to avoid infinite loop.
        | Option.all opt -> do
          Utils.listContents opt {Option.all = False, Option.almostAll = True} path >>= \case
            Left errMsg -> IO.hPrint IO.stderr errMsg >> pure []
            Right contents' -> pure contents'
        | otherwise ->
          Utils.listContents opt path >>= \case
            Left errMsg -> IO.hPrint IO.stderr errMsg >> pure []
            Right contents' -> pure contents'

  let pList = makeSomeNewPositionsList (length contents) $ Node.getTreeNodePositions node
  (newInodeSet, nodes) <- Recursive.excludeAlreadySeenInode inodeSet <$> mapM (Node.nodeInfo opt path) contents
  let nodes' = zipWith (\nd p -> nd {Node.getTreeNodePositions = p}) (Sort.sorter opt nodes) pList
      newNodeSeq = S.fromList nodes' S.>< nodeSeq

  (node S.<|) <$> makeTreeNodeInfos' newInodeSet opt' newNodeSeq
  where
    opt' = opt {Option.level = Depth.decreaseDepth depth}
    depth = Option.level opt
