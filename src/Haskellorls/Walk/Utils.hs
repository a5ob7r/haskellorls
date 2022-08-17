module Haskellorls.Walk.Utils
  ( Inodes,
    singleton,
    filterNodes,
  )
where

import Control.Monad.State.Strict (State, gets, put)
import qualified Data.IntSet as IS
import Data.List (foldl')
import qualified Haskellorls.NodeInfo as Node
import System.Posix.Types (FileID)

-- | Already traversed 'FileID's.
newtype Inodes = Inodes {unInodes :: IS.IntSet}
  deriving (Semigroup, Monoid)

singleton :: FileID -> Inodes
singleton = Inodes . IS.singleton . fromIntegral

-- | Filter a list of 'NodeInfo' to get newers which their 'FileID's haven't
-- been seen yet and remember them as a state.
filterNodes :: [Node.NodeInfo] -> State Inodes [Node.NodeInfo]
filterNodes nodes = do
  inodes <- gets unInodes

  let newers = filter ((`IS.notMember` inodes) . fromIntegral . Node.fileID) nodes
  put . Inodes . foldl' (flip IS.insert) inodes $ fromIntegral . Node.fileID <$> newers

  return newers
