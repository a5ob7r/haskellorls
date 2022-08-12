module Haskellorls.Formatter.Context
  ( context,
    normalColorizedContext,
    colorizedContext,
    module Haskellorls.Config.Context,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Context
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node

context :: Node.NodeInfo -> T.Text
context = Node.getNodeContext

-- | A node SELinux context formatter for the @no@ parameter of the @LS_COLORS@.
normalColorizedContext :: Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColorizedContext lscolors node = [Attr.Other $ WT.wrap lscolors getter cont]
  where
    cont = context node
    getter = Color.normal

colorizedContext :: Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
colorizedContext lscolors node = [Attr.Other $ WT.wrap lscolors getter cont]
  where
    cont = context node
    getter = Color.lookup $ FileContext cont
