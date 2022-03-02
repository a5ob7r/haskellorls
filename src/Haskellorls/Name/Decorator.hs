module Haskellorls.Name.Decorator
  ( colorizedNodeName,
    colorizedNodeNameWrapper,
    nodeNameWrapper,
    nodeName,
  )
where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT

colorizedNodeNameWrapper :: Option.Option -> Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
colorizedNodeNameWrapper opt config nd = Quote.quote (Quote.quoteStyle opt) $ colorizedNodeName opt config nd

colorizedNodeName :: Option.Option -> Color.LsColors -> Node.NodeInfo -> WT.WrappedText
colorizedNodeName opt c@(Color.Options {..}) nd = WT.WrappedText (left' <> l <> right') name' (left' <> r <> right')
  where
    name = nodeName nd
    name' = Utils.escapeFormatter opt name
    left' = Color.unSequence $ Maybe.fromMaybe "" left
    right' = Color.unSequence $ Maybe.fromMaybe "" right
    l = Maybe.maybe "" Color.unSequence $ nd `Color.lookup` c
    r = Color.unSequence $ Maybe.fromMaybe "" reset

nodeNameWrapper :: Option.Option -> Node.NodeInfo -> [WT.WrappedText]
nodeNameWrapper opt node = Quote.quote style . WT.toWrappedText $ Utils.escapeFormatter opt name
  where
    name = nodeName node
    style = Quote.quoteStyle opt

nodeName :: Node.NodeInfo -> T.Text
nodeName = T.pack . Node.getNodePath
