module Haskellorls.Context
  ( context,
    colorizedContext,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT

context :: Node.NodeInfo -> T.Text
context = Node.getNodeContext

colorizedContext :: Color.Config -> Node.NodeInfo -> [WT.WrappedText]
colorizedContext config node = [Color.toWrappedText config getter cont]
  where
    cont = context node
    getter = Color.fileContextEscapeSequence . Color.extensionColorConfig
