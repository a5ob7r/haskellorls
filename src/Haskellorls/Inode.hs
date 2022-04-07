module Haskellorls.Inode
  ( nodeInodeNumber,
    nodeInodeNumberWithNormalColor,
    nodeInodeNumberWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Inode.Type
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Types as Types

nodeInodeNumber :: Node.NodeInfo -> Types.FileID
nodeInodeNumber = Node.pfsFileID . Node.getNodeStatus

-- | A node inode number decorator with the @no@ parameter of the @LS_COLORS@.
nodeInodeNumberWithNormalColor :: Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
nodeInodeNumberWithNormalColor lscolors node = [Color.toWrappedText lscolors getterInode inodeNumber]
  where
    n = nodeInodeNumber node
    inodeNumber = T.pack $ show n
    getterInode = Color.normal

nodeInodeNumberWithColor :: Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
nodeInodeNumberWithColor lscolors node = [Color.toWrappedText lscolors getterInode inodeNumber]
  where
    n = nodeInodeNumber node
    inodeNumber = T.pack $ show n
    getterInode = Color.lookup $ Inode n
