module Haskellorls.Icon
  ( lookupIcon,
  )
where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT

lookupIcon :: Node.NodeInfo -> Color.LsIcons -> [WT.WrappedText]
lookupIcon node lsicons =
  [ WT.toWrappedText icon,
    WT.toWrappedText margin
  ]
  where
    icon = Maybe.maybe "" Color.unIcon $ node `Color.lookup` lsicons

margin :: T.Text
margin = " "
