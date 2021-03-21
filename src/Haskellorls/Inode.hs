module Haskellorls.Inode
  ( nodeInodeNumber,
    nodeInodeNumberWithColor,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Types as Types

nodeInodeNumber :: Node.NodeInfo -> Types.FileID
nodeInodeNumber = Node.pfsFileID . Node.getNodeStatus

nodeInodeNumberWithColor :: Color.Config -> Node.NodeInfo -> [WT.WrappedText]
nodeInodeNumberWithColor config node = [Color.toWrappedText config getterInode inodeNumber]
  where
    inodeNumber = T.pack . show $ nodeInodeNumber node
    getterInode = Color.fileInodeEscapeSequence . Color.extensionColorConfig
