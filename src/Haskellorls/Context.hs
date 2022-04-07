module Haskellorls.Context
  ( context,
    normalColorizedContext,
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

-- | A node SELinux context decorator for the @no@ parameter of the @LS_COLORS@.
normalColorizedContext :: Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
normalColorizedContext lscolors node = [Color.toWrappedText lscolors getter cont]
  where
    cont = context node
    getter = Color.normal

colorizedContext :: Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
colorizedContext lscolors node = [Color.toWrappedText lscolors getter cont]
  where
    cont = context node
    getter = Color.lookup $ FileContext cont
