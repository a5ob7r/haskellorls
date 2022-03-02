module Haskellorls.Context
  ( context,
    colorizedContext,
    module Haskellorls.Context.Type,
  )
where

import qualified Data.Text as T
import Haskellorls.Context.Type
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT

context :: Node.NodeInfo -> T.Text
context = Node.getNodeContext

colorizedContext :: Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
colorizedContext lscolors node = [Color.toWrappedText lscolors getter cont]
  where
    cont = context node
    getter = Color.lookup $ FileContext cont
