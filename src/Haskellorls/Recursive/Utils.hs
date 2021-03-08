module Haskellorls.Recursive.Utils
  ( InodeSet (..),
    excludeAlreadySeenInode,
  )
where

import qualified Data.Set as S
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix as Files
import qualified System.Posix.Types as Types

newtype InodeSet = InodeSet (S.Set Types.FileID)

excludeAlreadySeenInode :: InodeSet -> [Node.NodeInfo] -> (InodeSet, [Node.NodeInfo])
excludeAlreadySeenInode (InodeSet inodeSet) nodes = (InodeSet $ S.union inodeSet newInodeSet, newNodes)
  where
    newInodes = filter (`S.notMember` inodeSet) $ map (Files.fileID . Node.nodeInfoStatus) nodes
    newInodeSet = S.fromList newInodes
    newNodes = filter ((`S.member` newInodeSet) . Files.fileID . Node.nodeInfoStatus) nodes
