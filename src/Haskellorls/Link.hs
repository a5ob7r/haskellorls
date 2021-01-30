module Haskellorls.Link
  ( nodeLinksNumber,
    nodeLinksNumberWithColor,
  )
where

import qualified Haskellorls.Color as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.YetAnotherString as YAString
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

nodeLinksNumber :: Node.NodeInfo -> Types.LinkCount
nodeLinksNumber = Files.linkCount . Node.nodeInfoStatus

nodeLinksNumberWithColor :: Color.Config -> Node.NodeInfo -> [YAString.WrapedString]
nodeLinksNumberWithColor config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config linkEscSeq,
        YAString.wrappedStringMain = show linksNumber,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config resetEscSeq
      }
  ]
  where
    linksNumber = nodeLinksNumber node
    linkEscSeq = Color.fileLinkEscapeSequence $ Color.extensionColorConfig config
    resetEscSeq = Color.resetEscapeSequence config
