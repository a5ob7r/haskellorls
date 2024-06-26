module Haskellorls.Walk.Utils
  ( Inodes,
    singleton,
    filterNodes,
  )
where

import Control.Monad.State.Strict (State, gets, put)
import Data.IntSet qualified as IS
import Haskellorls.NodeInfo qualified as Node
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

  let newers = filter (maybe True ((`IS.notMember` inodes) . fromIntegral) . Node.fileID) nodes
  put . Inodes . foldl' (flip IS.insert) inodes $ maybe 0 fromIntegral . Node.fileID <$> newers

  return newers
