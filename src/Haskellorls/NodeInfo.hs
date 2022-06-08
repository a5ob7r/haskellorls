module Haskellorls.NodeInfo
  ( NodeType (..),
    NodeInfo (..),
    LinkNodeInfo (..),
    ProxyFileStatus (..),
    isDirectory,
    nodeInfo,
    toFileInfo,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Either.Extra as E
import Data.Functor
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Clock
import Haskellorls.Class
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Tree.Type as Tree
import qualified Haskellorls.Utils as Utils
import qualified System.FilePath.Posix as Posix
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

#ifdef SELINUX
import qualified Control.Exception.Base as Exception
import qualified Data.Either as E
import qualified System.Linux.SELinux as SELinux
#endif

data NodeType
  = Directory
  | SymbolicLink
  | NamedPipe
  | Socket
  | BlockDevise
  | CharDevise
  | DoorsDevise -- NOTE: Doors device is not implemented on Linux
  | Setuid
  | Setgid
  | Sticky
  | StickyOtherWritable
  | OtherWritable
  | Executable
  | File
  | Orphan
  deriving (Show)

data ProxyFileStatus = ProxyFileStatus
  { pfsFileMode :: Types.FileMode,
    pfsFileID :: Types.FileID,
    pfsLinkCount :: Types.LinkCount,
    pfsUserID :: Types.UserID,
    pfsGroupID :: Types.GroupID,
    pfsFileSize :: Types.FileOffset,
    pfsModificationTime :: Clock.POSIXTime,
    pfsAccessTime :: Clock.POSIXTime,
    pfsStatusChangeTime :: Clock.POSIXTime,
    pfsNodeType :: NodeType
  }

instance From ProxyFileStatus Types.UserID where
  from = pfsUserID

instance From ProxyFileStatus Types.GroupID where
  from = pfsGroupID

proxyFileStatus :: Files.FileStatus -> ProxyFileStatus
proxyFileStatus status =
  ProxyFileStatus
    { pfsFileMode = Files.fileMode status,
      pfsFileID = Files.fileID status,
      pfsLinkCount = Files.linkCount status,
      pfsUserID = Files.fileOwner status,
      pfsGroupID = Files.fileGroup status,
      pfsFileSize = Files.fileSize status,
      pfsModificationTime = Files.modificationTimeHiRes status,
      pfsAccessTime = Files.accessTimeHiRes status,
      pfsStatusChangeTime = Files.statusChangeTimeHiRes status,
      pfsNodeType = nodeTypeOf status
    }

nodeTypeOf :: Files.FileStatus -> NodeType
nodeTypeOf status
  | Files.isRegularFile status = regularFileNodeTypeOf status
  | Files.isDirectory status = directoryNodeTypeOf status
  | Files.isSymbolicLink status = SymbolicLink
  | Files.isNamedPipe status = NamedPipe
  | Files.isSocket status = Socket
  | Files.isBlockDevice status = BlockDevise
  | Files.isCharacterDevice status = CharDevise
  | otherwise = Orphan

regularFileNodeTypeOf :: Files.FileStatus -> NodeType
regularFileNodeTypeOf status
  | isSetuidMode mode = Setuid
  | isSetgidMode mode = Setgid
  | isExecutableMode mode = Executable
  | otherwise = File
  where
    mode = Files.fileMode status

directoryNodeTypeOf :: Files.FileStatus -> NodeType
directoryNodeTypeOf status
  | isStickyOtherWrite mode = StickyOtherWritable
  | isOtherWriteMode mode = OtherWritable
  | isStickyMode mode = Sticky
  | otherwise = Directory
  where
    mode = Files.fileMode status

isDirectory :: NodeType -> Bool
isDirectory = \case
  StickyOtherWritable -> True
  OtherWritable -> True
  Sticky -> True
  Directory -> True
  _ -> False

hasFileMode :: Types.FileMode -> Types.FileMode -> Bool
hasFileMode x y = x == Files.intersectFileModes x y

isOwnerExecuteMode :: Types.FileMode -> Bool
isOwnerExecuteMode = hasFileMode Files.ownerExecuteMode

isGroupExecuteMode :: Types.FileMode -> Bool
isGroupExecuteMode = hasFileMode Files.groupExecuteMode

isOtherWriteMode :: Types.FileMode -> Bool
isOtherWriteMode = hasFileMode Files.otherWriteMode

isOtherExecuteMode :: Types.FileMode -> Bool
isOtherExecuteMode = hasFileMode Files.otherExecuteMode

isExecutableMode :: Types.FileMode -> Bool
isExecutableMode = or . sequence [isOwnerExecuteMode, isGroupExecuteMode, isOtherExecuteMode]

isSetuidMode :: Types.FileMode -> Bool
isSetuidMode = hasFileMode Files.setUserIDMode

isSetgidMode :: Types.FileMode -> Bool
isSetgidMode = hasFileMode Files.setGroupIDMode

isStickyMode :: Types.FileMode -> Bool
isStickyMode = hasFileMode stickyMode

isStickyOtherWrite :: Types.FileMode -> Bool
isStickyOtherWrite = hasFileMode stickyOtherWriteMode

stickyMode :: Types.FileMode
stickyMode = 548

stickyOtherWriteMode :: Types.FileMode
stickyOtherWriteMode = Files.unionFileModes stickyMode Files.otherWriteMode

data NodeInfo = NodeInfo
  { getNodePath :: FilePath,
    getNodeStatus :: ProxyFileStatus,
    getNodeContext :: T.Text,
    getNodeDirName :: FilePath,
    getNodeLinkInfo :: Maybe (Either OrphanedLinkNodeInfo LinkNodeInfo),
    getTreeNodePositions :: [Tree.TreeNodePosition]
  }

instance From NodeInfo Types.UserID where
  from = from . getNodeStatus

instance From NodeInfo Types.GroupID where
  from = from . getNodeStatus

data LinkNodeInfo = LinkNodeInfo
  { getLinkNodePath :: FilePath,
    getLinkNodeStatus :: ProxyFileStatus,
    getLinkNodeContext :: T.Text
  }

newtype OrphanedLinkNodeInfo = OrphanedLinkNodeInfo {getOrphanedNodeLinkPath :: FilePath}

-- | Create a filenode infomation from a filepath.
nodeInfo :: (MonadCatch m, MonadIO m) => Option.Option -> FilePath -> FilePath -> m NodeInfo
nodeInfo opt dirname basename = do
  status <- Utils.getSymbolicLinkStatus path
  context <- fileContext path

  if Files.isSymbolicLink status
    then do
      linkPath <- tryIO $ Utils.readSymbolicLink path

      destStatus <- do
        case linkPath of
          -- Dereference file status if a status presents symbolic link.
          Right p -> tryIO (Utils.destFileStatusRecursive path p) <&> E.eitherToMaybe
          _ -> pure Nothing

      destContext <- do
        case linkPath of
          Right p -> fileContext p
          _ -> pure defaultContext

      return $ case (linkPath, destStatus) of
        (Right p, Nothing) ->
          NodeInfo
            { getNodePath = basename,
              getNodeStatus = proxyFileStatus status,
              getNodeContext = T.pack context,
              getNodeDirName = dirname,
              getNodeLinkInfo = Just . Left $ OrphanedLinkNodeInfo p,
              getTreeNodePositions = []
            }
        (Right p, Just s)
          | Option.dereference opt
              || Option.dereferenceCommandLine opt
              || (Option.dereferenceCommandLineSymlinkToDir opt && Files.isDirectory s) ->
              NodeInfo
                { getNodePath = basename,
                  getNodeStatus = proxyFileStatus s,
                  getNodeContext = T.pack context,
                  getNodeDirName = dirname,
                  getNodeLinkInfo = Nothing,
                  getTreeNodePositions = []
                }
          | otherwise ->
              NodeInfo
                { getNodePath = basename,
                  getNodeStatus = proxyFileStatus status,
                  getNodeContext = T.pack context,
                  getNodeDirName = dirname,
                  getNodeLinkInfo =
                    Just . Right $
                      LinkNodeInfo
                        { getLinkNodePath = p,
                          getLinkNodeStatus = proxyFileStatus s,
                          getLinkNodeContext = T.pack destContext
                        },
                  getTreeNodePositions = []
                }
        _ ->
          NodeInfo
            { getNodePath = basename,
              getNodeStatus = proxyFileStatus status,
              getNodeContext = T.pack context,
              getNodeDirName = dirname,
              getNodeLinkInfo = Nothing,
              getTreeNodePositions = []
            }
    else
      return $
        NodeInfo
          { getNodePath = basename,
            getNodeStatus = proxyFileStatus status,
            getNodeContext = T.pack context,
            getNodeDirName = dirname,
            getNodeLinkInfo = Nothing,
            getTreeNodePositions = []
          }
  where
    path = dirname Posix.</> basename

toFileInfo :: NodeInfo -> NodeInfo
toFileInfo node@NodeInfo {..} = case getNodeLinkInfo of
  Nothing -> node
  Just (Right LinkNodeInfo {..}) ->
    NodeInfo
      { getNodePath = getLinkNodePath,
        getNodeStatus = getLinkNodeStatus,
        getNodeContext = getLinkNodeContext,
        ..
      }
  Just (Left OrphanedLinkNodeInfo {..}) ->
    NodeInfo
      { getNodePath = getOrphanedNodeLinkPath,
        ..
      }

{- ORMOLU_DISABLE -}
-- |
-- NOTE: This is not tested on SELinux enabled environment so maybe break.
-- NOTE: Disable ormolu because it does not support CPP extension.
fileContext :: MonadIO m => FilePath -> m String
#ifdef SELINUX
fileContext path =
  do
    context <- tryIO (SELinux.getFileCon path)
    pure $ E.fromRight defaultContext context
#else
fileContext _ = pure defaultContext
#endif
{- ORMOLU_ENABLE -}

defaultContext :: String
defaultContext = "?"
