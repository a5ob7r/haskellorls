module Haskellorls.Name.Decorator
  ( colorizedNodeName,
    colorizedNodeNameWrapper,
    nodeNameWrapper,
    nodeName,
  )
where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Haskellorls.Class
import qualified Haskellorls.Hyperlink.Type as Hyperlink
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import System.FilePath.Posix.ByteString

colorizedNodeNameWrapper :: Option.Option -> Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
colorizedNodeNameWrapper opt config nd = Quote.quote (Quote.quoteStyle opt) $ colorizedNodeName opt config nd

colorizedNodeName :: Option.Option -> Color.LsColors -> Node.NodeInfo -> WT.WrappedText
colorizedNodeName opt c@(Color.Options {..}) nd = WT.WrappedText (left' <> l <> right' <> wtPrefix) wtWord (wtSuffix <> left' <> r <> right')
  where
    WT.WrappedText {..} = WT.modify (Utils.escapeFormatter opt) $ nodeName opt nd
    left' = Color.unSequence $ fromMaybe "" left
    right' = Color.unSequence $ fromMaybe "" right
    l = maybe "" Color.unSequence $ nd `Color.lookup` c
    r = Color.unSequence $ fromMaybe "" reset

nodeNameWrapper :: Option.Option -> Node.NodeInfo -> [WT.WrappedText]
nodeNameWrapper opt node = Quote.quote style $ WT.modify (Utils.escapeFormatter opt) . nodeName opt $ node
  where
    style = Quote.quoteStyle opt

nodeName :: Option.Option -> Node.NodeInfo -> WT.WrappedText
nodeName opt@(Option.Option {hyperlink, hostname, toStdout}) node =
  case hyperlink of
    Hyperlink.NEVER -> deserialize $ rawNodeName node
    Hyperlink.ALWAYS -> WT.WrappedText (left <> uri <> right) (rawNodeName node) (left <> right)
      where
        left = "\^[]8;;"
        right = "\^G"
        uri = "file://" <> hostname <> T.decodeUtf8 (mkAbsolutePath opt node)
    Hyperlink.AUTO
      | toStdout -> nodeName opt {Option.hyperlink = Hyperlink.ALWAYS} node
      | otherwise -> nodeName opt {Option.hyperlink = Hyperlink.NEVER} node

rawNodeName :: Node.NodeInfo -> T.Text
rawNodeName = T.decodeUtf8 . Node.getNodePath

-- | Make the absolute path from a node.
mkAbsolutePath :: Option.Option -> Node.NodeInfo -> RawFilePath
mkAbsolutePath Option.Option {currentWorkingDirectory} Node.NodeInfo {..} =
  normalise $
    if
        | isAbsolute getNodePath -> getNodePath
        | isAbsolute getNodeDirName -> getNodeDirName </> getNodePath
        | otherwise -> currentWorkingDirectory </> getNodeDirName </> getNodePath
