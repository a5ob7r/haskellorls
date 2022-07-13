module Haskellorls.Tree.Util (mkTreeNodeInfos) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Either
import Data.Functor
import qualified Data.Sequence as S
import GHC.IO.Exception
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
  (y : ys) -> (HEAD : y) : mkSomeNewPositionsList' ys

mkSomeNewPositionsList' :: [[TreeNodePosition]] -> [[TreeNodePosition]]
mkSomeNewPositionsList' [] = []
mkSomeNewPositionsList' [x] = [LAST : x]
mkSomeNewPositionsList' (x : xs) = (MID : x) : mkSomeNewPositionsList' xs

mkTreeNodeInfos :: (MonadCatch m, MonadIO m) => Option.Option -> Node.NodeInfo -> m (S.Seq Node.NodeInfo, [SomeException])
mkTreeNodeInfos opt node = do
  let inodes = Recursive.singletonInodes $ Node.fileID node

  mkTreeNodeInfos' inodes opt (S.singleton node) []

-- | With error message output.
mkTreeNodeInfos' :: (MonadCatch m, MonadIO m) => Recursive.AlreadySeenInodes -> Option.Option -> S.Seq Node.NodeInfo -> [SomeException] -> m (S.Seq Node.NodeInfo, [SomeException])
mkTreeNodeInfos' _ _ S.Empty _ = pure mempty
mkTreeNodeInfos' inodes opt (node S.:<| nodeSeq) errors = do
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

  -- Maybe contain nodeinfos which the inode number is already seen.
  (errs, nodeinfos) <- partitionEithers <$> mapM (tryIO . Node.mkNodeInfo opt path) contents

  -- NOTE: In this application, and probably almost all applications,
  -- traversing directory contents and getting each file statuses are
  -- non-atomic. So maybe causes looking up nonexistence filepaths. In such
  -- case, ignore nonexistence filepaths.
  let errs' = toException <$> filter (\e -> ioe_type e /= NoSuchThing) errs

  let (nodes, newInodes) = runState (Recursive.updateAlreadySeenInode nodeinfos) inodes
      pList = makeSomeNewPositionsList (length nodes) $ Node.getTreeNodePositions node
      nodes' = zipWith (\nd p -> nd {Node.getTreeNodePositions = p}) (Sort.sorter opt nodes) pList
      newNodeSeq = S.fromList nodes' <> nodeSeq

  mkTreeNodeInfos' newInodes opt' newNodeSeq (errs' <> errors) <&> first (node S.<|)
  where
    opt' = opt {Option.level = Depth.decreaseDepth depth}
    depth = Option.level opt
