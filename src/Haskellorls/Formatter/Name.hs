module Haskellorls.Formatter.Name
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
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.Escape as Escape
import qualified Haskellorls.Formatter.Quote as Quote
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import System.FilePath.Posix.ByteString

colorizedNodeNameWrapper :: Config.Config -> Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
colorizedNodeNameWrapper config lc nd = Quote.quote (Quote.quoteStyle config) $ colorizedNodeName config lc nd

colorizedNodeName :: Config.Config -> Color.LsColors -> Node.NodeInfo -> Attr.Attribute WT.WrappedText
colorizedNodeName config c@(Color.Options {..}) nd = Attr.Name $ WT.WrappedText (left' <> l <> right' <> wtPrefix) wtWord (wtSuffix <> left' <> r <> right')
  where
    WT.WrappedText {..} = Attr.unwrap $ WT.modify (Escape.escapeFormatter config) <$> nodeName config nd
    left' = Color.unSequence $ fromMaybe "" left
    right' = Color.unSequence $ fromMaybe "" right
    l = maybe "" Color.unSequence $ nd `Color.lookup` c
    r = Color.unSequence $ fromMaybe "" reset

nodeNameWrapper :: Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
nodeNameWrapper config node = Quote.quote (Quote.quoteStyle config) $ WT.modify (Escape.escapeFormatter config) <$> nodeName config node

nodeName :: Config.Config -> Node.NodeInfo -> Attr.Attribute WT.WrappedText
nodeName config@(Config.Config {hyperlink, hostname}) node =
  if hyperlink
    then Attr.Name $ WT.WrappedText (left <> uri <> right) (rawNodeName node) (left <> right)
    else Attr.Name $ deserialize $ rawNodeName node
  where
    left = "\^[]8;;"
    right = "\^G"
    uri = "file://" <> hostname <> T.decodeUtf8 (mkAbsolutePath config node)

rawNodeName :: Node.NodeInfo -> T.Text
rawNodeName = T.decodeUtf8 . Node.getNodePath

-- | Make the absolute path from a node.
mkAbsolutePath :: Config.Config -> Node.NodeInfo -> RawFilePath
mkAbsolutePath Config.Config {currentWorkingDirectory} Node.NodeInfo {..} =
  normalise $
    if
        | isAbsolute getNodePath -> getNodePath
        | isAbsolute getNodeDirName -> getNodeDirName </> getNodePath
        | otherwise -> currentWorkingDirectory </> getNodeDirName </> getNodePath
