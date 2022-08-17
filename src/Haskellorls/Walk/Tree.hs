module Haskellorls.Walk.Tree (mkTreeNodeInfos) where

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Either
import Data.Functor
import qualified Data.Sequence as S
import GHC.IO.Exception
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Listing
import Haskellorls.Config.Tree
import Haskellorls.Control.Exception.Safe
import Haskellorls.Data.Infinitable
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Walk.Listing as Listing
import qualified Haskellorls.Walk.Sort as Sort
import qualified Haskellorls.Walk.Utils as Walk
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

mkTreeNodeInfos :: (MonadCatch m, MonadIO m) => Config.Config -> Node.NodeInfo -> m (S.Seq Node.NodeInfo, [SomeException])
mkTreeNodeInfos config node = do
  let inodes = Walk.singleton $ Node.fileID node

  mkTreeNodeInfos' inodes config (S.singleton node) []

-- | With error message output.
mkTreeNodeInfos' :: (MonadCatch m, MonadIO m) => Walk.Inodes -> Config.Config -> S.Seq Node.NodeInfo -> [SomeException] -> m (S.Seq Node.NodeInfo, [SomeException])
mkTreeNodeInfos' _ _ S.Empty _ = pure mempty
mkTreeNodeInfos' inodes config (node S.:<| nodeSeq) errors = do
  let path = Node.getNodeDirName node </> Node.getNodePath node
  contents <- case Config.listing config of
    _ | depth <= Only 0 || not (Node.isDirectory $ Node.nodeType node) -> pure []
    -- Force hide '.' and '..' to avoid infinite loop.
    All ->
      tryIO (Listing.listContents config {Config.listing = AlmostAll} path) >>= \case
        Left errMsg -> printErr errMsg >> pure []
        Right contents' -> pure contents'
    _ ->
      tryIO (Listing.listContents config path) >>= \case
        Left errMsg -> printErr errMsg >> pure []
        Right contents' -> pure contents'

  -- Maybe contain nodeinfos which the inode number is already seen.
  (errs, nodeinfos) <- partitionEithers <$> mapM (tryIO . Node.mkNodeInfo config path) contents

  -- NOTE: In this application, and probably almost all applications,
  -- traversing directory contents and getting each file statuses are
  -- non-atomic. So maybe causes looking up nonexistence filepaths. In such
  -- case, ignore nonexistence filepaths.
  let errs' = toException <$> filter (\e -> ioe_type e /= NoSuchThing) errs

  let (nodes, newInodes) = runState (Walk.filterNodes nodeinfos) inodes
      pList = makeSomeNewPositionsList (length nodes) $ Node.getTreeNodePositions node
      nodes' = zipWith (\nd p -> nd {Node.getTreeNodePositions = p}) (Sort.sort config nodes) pList
      newNodeSeq = S.fromList nodes' <> nodeSeq

  mkTreeNodeInfos' newInodes config' newNodeSeq (errs' <> errors) <&> first (node S.<|)
  where
    config' = config {Config.level = pred <$> depth}
    depth = Config.level config
