module Haskellorls.Link
  ( nodeLinksNumber,
    nodeLinksNumberWithColor,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

nodeLinksNumber :: Node.NodeInfo -> Types.LinkCount
nodeLinksNumber = Files.linkCount . Node.nodeInfoStatus

nodeLinksNumberWithColor :: Color.Config -> Node.NodeInfo -> [WT.WrappedText]
nodeLinksNumberWithColor config node = [Color.toWrappedText config getter linksNumber]
  where
    linksNumber = T.pack . show $ nodeLinksNumber node
    getter = Color.fileLinkEscapeSequence . Color.extensionColorConfig
