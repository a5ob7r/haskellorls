module Haskellorls.Formatter.Link
  ( nodeLinksNumber,
    nodeLinksNumberWithNormalColor,
    nodeLinksNumberWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Link
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types

nodeLinksNumber :: Node.NodeInfo -> Types.LinkCount
nodeLinksNumber = Node.linkCount

-- | A number of node links formatter with the @no@ parameter of the @LS_COLORS@.
nodeLinksNumberWithNormalColor :: Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
nodeLinksNumberWithNormalColor lscolors node = [Attr.Other $ WT.wrap lscolors getter linksNumber]
  where
    linksNumber = T.pack . show $ nodeLinksNumber node
    getter = Color.normal

nodeLinksNumberWithColor :: Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
nodeLinksNumberWithColor lscolors node = [Attr.Other $ WT.wrap lscolors getter linksNumber]
  where
    linksNumber = T.pack . show $ nodeLinksNumber node
    getter = Color.lookup . LinkCount $ nodeLinksNumber node
