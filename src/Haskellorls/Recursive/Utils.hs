module Haskellorls.Recursive.Utils
  ( AlreadySeenInodes,
    emptyInodes,
    singletonInodes,
    updateAlreadySeenInode,
  )
where

import qualified Control.Monad.State.Strict as State
import qualified Data.Set as S
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types

newtype AlreadySeenInodes = AlreadySeenInodes (S.Set Types.FileID)

emptyInodes :: AlreadySeenInodes
emptyInodes = AlreadySeenInodes S.empty

singletonInodes :: Types.FileID -> AlreadySeenInodes
singletonInodes = AlreadySeenInodes . S.singleton

-- | Update already seen inode number set.
updateAlreadySeenInode :: [Node.NodeInfo] -> State.State AlreadySeenInodes [Node.NodeInfo]
updateAlreadySeenInode nodes = do
  -- Get already seen inode numbers set from the environment.
  AlreadySeenInodes inodes <- State.get

  -- Select all node info which the inode number is not already seen.
  let neverSeenNodes = filter ((`S.notMember` inodes) . getInodeNumber) nodes
      neverSeenInodes = map getInodeNumber neverSeenNodes

  -- Update already seen inode numbers set.
  State.put . AlreadySeenInodes . S.union inodes $ S.fromList neverSeenInodes

  pure neverSeenNodes

getInodeNumber :: Node.NodeInfo -> Types.FileID
getInodeNumber = Node.pfsFileID . Node.getNodeStatus
