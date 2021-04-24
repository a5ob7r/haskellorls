module Haskellorls.Recursive.Utils
  ( InodeSet (..),
    updateAlreadySeenInode,
  )
where

import qualified Control.Monad.State.Strict as State
import qualified Data.Set as S
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types

newtype InodeSet = InodeSet (S.Set Types.FileID)

-- | Update already seen inode number set.
updateAlreadySeenInode :: [Node.NodeInfo] -> State.State InodeSet [Node.NodeInfo]
updateAlreadySeenInode nodes = do
  -- Get already seen inode numbers set from the environment.
  InodeSet inodeSet <- State.get

  -- Select all node info which the inode number is not already seen.
  let neverSeenNodes = filter ((`S.notMember` inodeSet) . getInodeNumber) nodes
      neverSeenInodes = map getInodeNumber neverSeenNodes

  -- Update already seen inode numbers set.
  State.put . InodeSet . S.union inodeSet $ S.fromList neverSeenInodes

  pure neverSeenNodes

getInodeNumber :: Node.NodeInfo -> Types.FileID
getInodeNumber = Node.pfsFileID . Node.getNodeStatus
