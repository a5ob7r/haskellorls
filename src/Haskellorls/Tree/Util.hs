module Haskellorls.Tree.Util
  ( makeSomeNewPositionsList,
    makeTreeNodeInfos,
  )
where

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import qualified Data.Sequence as S
import qualified Haskellorls.Depth as Depth
import Haskellorls.Exception
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Recursive.Utils as Recursive
import qualified Haskellorls.Sort.Method as Sort
import Haskellorls.Tree.Type
import qualified Haskellorls.Utils as Utils
import System.FilePath.Posix.ByteString

makeSomeNewPositionsList :: Int -> [TreeNodePosition] -> [[TreeNodePosition]]
makeSomeNewPositionsList n _
  | n < 1 = [[]]
makeSomeNewPositionsList 1 xs = [LAST : xs]
makeSomeNewPositionsList n xs = case replicate n xs of
  [] -> []
  (y : ys) -> (HEAD : y) : makeSomeNewPositionsList' ys

makeSomeNewPositionsList' :: [[TreeNodePosition]] -> [[TreeNodePosition]]
makeSomeNewPositionsList' [] = []
makeSomeNewPositionsList' [x] = [LAST : x]
makeSomeNewPositionsList' (x : xs) = (MID : x) : makeSomeNewPositionsList' xs

makeTreeNodeInfos :: (MonadCatch m, MonadIO m) => Option.Option -> RawFilePath -> m (S.Seq Node.NodeInfo)
makeTreeNodeInfos opt path = do
  node <- Node.mkNodeInfo opt "" path
  let inodes = Recursive.singletonInodes $ Node.fileID node
  makeTreeNodeInfos' inodes opt $ S.singleton node

-- | With error message output.
makeTreeNodeInfos' :: (MonadCatch m, MonadIO m) => Recursive.AlreadySeenInodes -> Option.Option -> S.Seq Node.NodeInfo -> m (S.Seq Node.NodeInfo)
makeTreeNodeInfos' _ _ S.Empty = pure mempty
makeTreeNodeInfos' inodes opt (node S.:<| nodeSeq) = do
  let path = Node.getNodeDirName node </> Node.getNodePath node
  contents <-
    if
        | Depth.isDepthZero depth || not (Node.isDirectory $ Node.nodeType node) -> pure []
        -- Force hide '.' and '..' to avoid infinite loop.
        | Option.all opt -> do
            tryIO (Utils.listContents opt {Option.all = False, Option.almostAll = True} path) >>= \case
              Left errMsg -> liftIO (printErr errMsg) >> pure []
              Right contents' -> pure contents'
        | otherwise ->
            tryIO (Utils.listContents opt path) >>= \case
              Left errMsg -> liftIO (printErr errMsg) >> pure []
              Right contents' -> pure contents'

  let pList = makeSomeNewPositionsList (length contents) $ Node.getTreeNodePositions node

  -- Maybe contain nodeinfos which the inode number is already seen.
  nodeinfos <- mapM (Node.mkNodeInfo opt path) contents

  let (nodes, newInodes) = runState (Recursive.updateAlreadySeenInode nodeinfos) inodes
      nodes' = zipWith (\nd p -> nd {Node.getTreeNodePositions = p}) (Sort.sorter opt nodes) pList
      newNodeSeq = S.fromList nodes' <> nodeSeq

  (node S.<|) <$> makeTreeNodeInfos' newInodes opt' newNodeSeq
  where
    opt' = opt {Option.level = Depth.decreaseDepth depth}
    depth = Option.level opt
