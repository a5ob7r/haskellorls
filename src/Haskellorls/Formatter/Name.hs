module Haskellorls.Formatter.Name
  ( colorizedNodeName,
    nodeName,
  )
where

import Haskellorls.Config qualified as Config
import Haskellorls.Formatter.Attribute qualified as Attr
import Haskellorls.Formatter.WrappedText qualified as WT
import Haskellorls.LsColor qualified as Color
import Haskellorls.NodeInfo qualified as Node
import Haskellorls.System.OsPath.Posix.Extra (PosixPath, decode, isAbsolute, normalise, (</>))
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
    name = from . decode $ Node.getNodePath node
    left = "\^[]8;;"
    right = "\^G"
    uri = "file://" <> hostname <> from (decode $ mkAbsolutePath config node)

-- | Make the absolute path from a node.
mkAbsolutePath :: Config.Config -> Node.NodeInfo -> PosixPath
mkAbsolutePath Config.Config {currentWorkingDirectory} Node.NodeInfo {..} = normalise path
  where
    path
      | isAbsolute getNodePath = getNodePath
      | isAbsolute getNodeDirName = getNodeDirName </> getNodePath
      | otherwise = currentWorkingDirectory </> getNodeDirName </> getNodePath
