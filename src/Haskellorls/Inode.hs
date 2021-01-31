module Haskellorls.Inode
  ( nodeInodeNumber,
    nodeInodeNumberWithColor,
  )
where

import qualified Haskellorls.Color as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.YetAnotherString as YAString
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

nodeInodeNumber :: Node.NodeInfo -> Types.FileID
nodeInodeNumber = Files.fileID . Node.nodeInfoStatus

nodeInodeNumberWithColor :: Color.Config -> Node.NodeInfo -> [YAString.WrapedString]
nodeInodeNumberWithColor config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config inodeEscSeq,
        YAString.wrappedStringMain = show inodeNumber,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config resetEscSeq
      }
  ]
  where
    inodeNumber = nodeInodeNumber node
    inodeEscSeq = Color.fileInodeEscapeSequence $ Color.extensionColorConfig config
    resetEscSeq = Color.resetEscapeSequence config
