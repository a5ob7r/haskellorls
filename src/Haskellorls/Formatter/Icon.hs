module Haskellorls.Formatter.Icon
  ( lookupIcon,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node

lookupIcon :: Node.NodeInfo -> Color.LsIcons -> [WT.WrappedText]
lookupIcon node lsicons =
  [ WT.deserialize icon,
    WT.deserialize margin
  ]
  where
    icon = maybe "" Color.unIcon $ node `Color.lookup` lsicons

margin :: T.Text
margin = " "
