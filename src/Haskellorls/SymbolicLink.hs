module Haskellorls.SymbolicLink
  ( linkName,
    linkNameWrapper,
    coloredLinkName,
  )
where

import qualified Haskellorls.Color as Color
import qualified Haskellorls.Name as Name
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.YetAnotherString as YAString

linkName :: Node.NodeInfo -> FilePath
linkName node = case node of
  Node.FileInfo {} -> ""
  _ -> prefix <> Node.getDestPath node

linkNameWrapper :: Node.NodeInfo -> [YAString.WrapedString]
linkNameWrapper node = if null link then [] else YAString.toWrappedStringArray link
  where
    link = linkName node

coloredLinkName :: Color.Config -> Node.NodeInfo -> [YAString.WrapedString]
coloredLinkName config node = case node of
  Node.FileInfo {} -> []
  _ -> prefix' <> output
  where
    output = Name.colorizedNodeName config $ Node.toFileInfo node
    prefix' = YAString.toWrappedStringArray prefix

prefix :: String
prefix = " -> "
