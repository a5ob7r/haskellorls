module Haskellorls.Formatter.Icon (lookupIcon) where

import qualified Data.Text as T
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import Witch (from)

lookupIcon :: Node.NodeInfo -> Color.LsIcons -> [Attr.Attribute WT.WrappedText]
lookupIcon node lsicons =
  [ Attr.Other $ from icon,
    Attr.Other $ from @T.Text " "
  ]
  where
    icon = maybe "" Color.unIcon $ node `Color.lookup` lsicons
