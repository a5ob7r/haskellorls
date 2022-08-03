module Haskellorls.Walk.Utils
  ( AlreadySeenInodes,
    singletonInodes,
    updateAlreadySeenInode,
  )
where

import Control.Monad.State.Strict
import qualified Data.Set as S
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types

newtype AlreadySeenInodes = AlreadySeenInodes (S.Set Types.FileID)
  deriving (Semigroup, Monoid)

singletonInodes :: Types.FileID -> AlreadySeenInodes
singletonInodes = AlreadySeenInodes . S.singleton

-- | Update already seen inode number set.
updateAlreadySeenInode :: [Node.NodeInfo] -> State AlreadySeenInodes [Node.NodeInfo]
updateAlreadySeenInode nodes = do
  -- Get already seen inode numbers set from the environment.
  AlreadySeenInodes inodes <- get

  -- Select all node info which the inode number is not already seen.
  let neverSeenNodes = filter ((`S.notMember` inodes) . Node.fileID) nodes
      neverSeenInodes = map Node.fileID neverSeenNodes

  -- Update already seen inode numbers set.
  put . AlreadySeenInodes . S.union inodes $ S.fromList neverSeenInodes

  pure neverSeenNodes
