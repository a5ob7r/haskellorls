module Haskellorls.Formatter.Inode
  ( nodeInodeNumber,
    nodeInodeNumberWithNormalColor,
    nodeInodeNumberWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Inode
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types

nodeInodeNumber :: Node.NodeInfo -> Types.FileID
nodeInodeNumber = Node.fileID

-- | A node inode number formatter with the @no@ parameter of the @LS_COLORS@.
nodeInodeNumberWithNormalColor :: Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
nodeInodeNumberWithNormalColor lscolors node = [Attr.Other $ WT.wrap lscolors getterInode inodeNumber]
  where
    n = nodeInodeNumber node
    inodeNumber = T.pack $ show n
    getterInode = Color.normal

nodeInodeNumberWithColor :: Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
nodeInodeNumberWithColor lscolors node = [Attr.Other $ WT.wrap lscolors getterInode inodeNumber]
  where
    n = nodeInodeNumber node
    inodeNumber = T.pack $ show n
    getterInode = Color.lookup $ Inode n
