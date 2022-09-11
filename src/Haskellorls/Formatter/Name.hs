module Haskellorls.Formatter.Name
  ( colorizedNodeName,
    nodeName,
  )
where

import qualified Data.Text.Encoding as T
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import System.FilePath.Posix.ByteString
import Witch (From (..))

colorizedNodeName :: Config.Config -> Color.LsColors -> Node.NodeInfo -> Attr.Attribute WT.WrappedText
colorizedNodeName config lscolors node = Attr.Name $ WT.WrappedText (prefix' <> prefix) t (suffix <> suffix')
  where
    WT.WrappedText prefix t suffix = Attr.unwrap $ nodeName config node
    WT.WrappedText prefix' _ suffix' = WT.wrap lscolors (node `Color.lookup`) t

nodeName :: Config.Config -> Node.NodeInfo -> Attr.Attribute WT.WrappedText
nodeName config@(Config.Config {hyperlink, hostname}) node
  | hyperlink = Attr.Name $ WT.WrappedText (left <> uri <> right) name (left <> right)
  | otherwise = Attr.Name $ from name
  where
    name = T.decodeUtf8 $ Node.getNodePath node
    left = "\^[]8;;"
    right = "\^G"
    uri = "file://" <> hostname <> T.decodeUtf8 (mkAbsolutePath config node)

-- | Make the absolute path from a node.
mkAbsolutePath :: Config.Config -> Node.NodeInfo -> RawFilePath
mkAbsolutePath Config.Config {currentWorkingDirectory} Node.NodeInfo {..} = normalise path
  where
    path
      | isAbsolute getNodePath = getNodePath
      | isAbsolute getNodeDirName = getNodeDirName </> getNodePath
      | otherwise = currentWorkingDirectory </> getNodeDirName </> getNodePath
