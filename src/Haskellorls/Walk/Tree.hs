module Haskellorls.Walk.Tree (mkTreeNodeInfos) where

import Control.Exception.Safe (MonadCatch, SomeException, toException, tryIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (runState)
import Data.Either (partitionEithers)
import Data.Sequence (Seq (Empty, (:<|)), fromList, singleton, (|>))
import GHC.IO.Exception (IOErrorType (NoSuchThing), IOException (ioe_type))
import Haskellorls.Class
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Listing
import Haskellorls.Config.Tree
import Haskellorls.Data.Infinitable
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Walk.Listing as Listing
import qualified Haskellorls.Walk.Sort as Sort
import qualified Haskellorls.Walk.Utils as Walk
import System.FilePath.Posix.ByteString ((</>))

mkSomeNewPositionsList :: Int -> [TreeNodePosition] -> [[TreeNodePosition]]
mkSomeNewPositionsList n _ | n < 1 = [[]]
mkSomeNewPositionsList 1 xs = [LAST : xs]
mkSomeNewPositionsList n xs = case replicate n xs of
  [] -> []
  (y : ys) -> (HEAD : y) : go ys
  where
    go [] = []
    go [y] = [LAST : y]
    go (y : ys) = (MID : y) : go ys

mkTreeNodeInfos :: forall m. (MonadCatch m, MonadIO m) => Config.Config -> Node.NodeInfo -> m (Seq Node.NodeInfo, [SomeException])
mkTreeNodeInfos configuration nodeinfo =
  let config
        | All <- Config.listing configuration = configuration {Config.listing = AlmostAll} -- Force hide @.@ and @..@ to avoid infinite loop.
        | otherwise = configuration
   in go (Walk.singleton $ Node.fileID nodeinfo) config (singleton nodeinfo) mempty []
  where
    go :: Walk.Inodes -> Config.Config -> Seq Node.NodeInfo -> Seq Node.NodeInfo -> [SomeException] -> m (Seq Node.NodeInfo, [SomeException])
    go _ _ Empty nodes errors = return (nodes, errors)
    go inodes config (node :<| nodeSeq) nodeSeq' errors = do
      let path = Node.getNodeDirName node </> Node.getNodePath node
          depth = Config.level config

      contents <-
        if depth <= Only 0 || not (Node.isDirectory $ Node.nodeType node)
          then return []
          else
            tryIO (Listing.listContents config path) >>= \case
              Left e -> notify e >> return []
              Right contents -> return contents

      -- Maybe contain nodeinfos which the inode number is already seen.
      (errs, nodeinfos) <- partitionEithers <$> mapM (tryIO . Node.mkNodeInfo config path) contents

      -- NOTE: In this application, and probably almost all applications,
      -- traversing directory contents and getting each file statuses are
      -- non-atomic. So maybe causes looking up nonexistence filepaths. In such
      -- case, ignore nonexistence filepaths.
      let errs' = toException <$> filter (\e -> ioe_type e /= NoSuchThing) errs
          (nodes, newInodes) = runState (Walk.filterNodes nodeinfos) inodes
          pList = mkSomeNewPositionsList (length nodes) $ Node.getTreeNodePositions node
          nodes' = zipWith (\nd p -> nd {Node.getTreeNodePositions = p}) (Sort.sort config nodes) pList

      go newInodes config {Config.level = pred <$> depth} (fromList nodes' <> nodeSeq) (nodeSeq' |> node) (errs' <> errors)
