module Haskellorls.Formatter.SymbolicLink
  ( linkName,
    coloredLinkName,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.Escape as Escape
import qualified Haskellorls.Formatter.Name as Name
import qualified Haskellorls.Formatter.Quote as Quote
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor.Color as Color
import qualified Haskellorls.NodeInfo as Node

linkName :: Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
linkName config node = case Node.getNodeLinkInfo node of
  Nothing -> []
  _ -> prefix' : Quote.quote style (WT.modify (Escape.escape config) <$> link)
  where
    style = Quote.quoteStyleForLink config
    link = Name.nodeName config $ Node.toFileInfo node
    prefix' = Attr.Other $ WT.deserialize prefix

coloredLinkName :: Config.Config -> Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredLinkName config lc node = case Node.getNodeLinkInfo node of
  Nothing -> []
  _ -> prefix' <> Quote.quote style link
  where
    style = Quote.quoteStyleForLink config
    link = Name.colorizedNodeName config lc $ Node.toFileInfo node
    prefix' = [Attr.Other $ WT.deserialize prefix]

prefix :: T.Text
prefix = " -> "
