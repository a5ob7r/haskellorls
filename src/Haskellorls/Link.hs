module Haskellorls.Link
  ( nodeLinksNumber,
    nodeLinksNumberWithNormalColor,
    nodeLinksNumberWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Link.Type
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Types as Types

nodeLinksNumber :: Node.NodeInfo -> Types.LinkCount
nodeLinksNumber = Node.linkCount

-- | A number of node links decorator with the @no@ parameter of the @LS_COLORS@.
nodeLinksNumberWithNormalColor :: Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
nodeLinksNumberWithNormalColor lscolors node = [Color.toWrappedText lscolors getter linksNumber]
  where
    linksNumber = T.pack . show $ nodeLinksNumber node
    getter = Color.normal

nodeLinksNumberWithColor :: Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
nodeLinksNumberWithColor lscolors node = [Color.toWrappedText lscolors getter linksNumber]
  where
    linksNumber = T.pack . show $ nodeLinksNumber node
    getter = Color.lookup . LinkCount $ nodeLinksNumber node
