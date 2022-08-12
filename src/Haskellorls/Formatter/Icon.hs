module Haskellorls.Formatter.Icon (lookupIcon) where

import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node

lookupIcon :: Node.NodeInfo -> Color.LsIcons -> [Attr.Attribute WT.WrappedText]
lookupIcon node lsicons =
  [ Attr.Other $ WT.deserialize icon,
    Attr.Other $ WT.deserialize " "
  ]
  where
    icon = maybe "" Color.unIcon $ node `Color.lookup` lsicons
