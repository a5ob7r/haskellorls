module Haskellorls.Formatter.SymbolicLink
  ( linkName,
    coloredLinkName,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.Name as Name
import qualified Haskellorls.Formatter.Quote as Quote
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor.Color as Color
import qualified Haskellorls.NodeInfo as Node
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
