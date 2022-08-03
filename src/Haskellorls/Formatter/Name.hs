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
import qualified Haskellorls.Formatter.Escape as Escape
import qualified Haskellorls.Formatter.Quote as Quote
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import System.FilePath.Posix.ByteString

colorizedNodeNameWrapper :: Config.Config -> Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
colorizedNodeNameWrapper config lc nd = Quote.quote (Quote.quoteStyle config) $ colorizedNodeName config lc nd

colorizedNodeName :: Config.Config -> Color.LsColors -> Node.NodeInfo -> WT.WrappedText
colorizedNodeName config c@(Color.Options {..}) nd = WT.WrappedText (left' <> l <> right' <> wtPrefix) wtWord (wtSuffix <> left' <> r <> right')
  where
    WT.WrappedText {..} = WT.modify (Escape.escapeFormatter config) $ nodeName config nd
    left' = Color.unSequence $ fromMaybe "" left
    right' = Color.unSequence $ fromMaybe "" right
    l = maybe "" Color.unSequence $ nd `Color.lookup` c
    r = Color.unSequence $ fromMaybe "" reset

nodeNameWrapper :: Config.Config -> Node.NodeInfo -> [WT.WrappedText]
nodeNameWrapper config = Quote.quote style . WT.modify (Escape.escapeFormatter config) . nodeName config
  where
    style = Quote.quoteStyle config

nodeName :: Config.Config -> Node.NodeInfo -> WT.WrappedText
nodeName config@(Config.Config {hyperlink, hostname}) node =
  if hyperlink
    then WT.WrappedText (left <> uri <> right) (rawNodeName node) (left <> right)
    else deserialize $ rawNodeName node
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
