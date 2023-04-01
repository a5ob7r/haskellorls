module Haskellorls.Formatter.SymbolicLink
  ( linkName,
    coloredLinkName,
  )
where

import Data.Text qualified as T
import Haskellorls.Config qualified as Config
import Haskellorls.Formatter.Attribute qualified as Attr
import Haskellorls.Formatter.Name qualified as Name
import Haskellorls.Formatter.Quote qualified as Quote
import Haskellorls.Formatter.WrappedText qualified as WT
import Haskellorls.LsColor.Color qualified as Color
import Haskellorls.NodeInfo qualified as Node
import Witch (from)

linkName :: Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
linkName config node = case Node.getNodeLinkInfo node of
  Nothing -> []
  Just linkinfo ->
    let link = Name.nodeName config $ (Node.dereference node) {Node.getNodePath = Node.dereferencedNodePath linkinfo}
     in Attr.Other (from prefix) : [Quote.quote config link]

coloredLinkName :: Config.Config -> Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredLinkName config lc node = case Node.getNodeLinkInfo node of
  Nothing -> []
  Just linkinfo ->
    let link = Name.colorizedNodeName config lc $ (Node.dereference node) {Node.getNodePath = Node.dereferencedNodePath linkinfo}
     in Attr.Other (from prefix) : [Quote.quote config link]

prefix :: T.Text
prefix = " -> "
