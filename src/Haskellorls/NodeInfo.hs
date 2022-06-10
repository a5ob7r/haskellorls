module Haskellorls.NodeInfo
  ( NodeType (..),
    NodeInfo (..),
    fileID,
    fileMode,
    linkCount,
    fileSize,
    userID,
    groupID,
    modificationTime,
    accessTime,
    changeTime,
    nodeType,
    isDirectory,
    mkNodeInfo,
    toFileInfo,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.Either.Extra
import Data.Functor
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Tree.Type as Tree
import qualified Haskellorls.Utils as Utils
import System.FilePath.Posix
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

#ifdef SELINUX
import Data.Either
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

mkNodeType :: Files.FileStatus -> NodeType
mkNodeType status
  | Files.isRegularFile status =
      if
          | isSetuidMode mode -> Setuid
          | isSetgidMode mode -> Setgid
          | isExecutableMode mode -> Executable
          | otherwise -> File
  | Files.isDirectory status =
      if
          | isStickyOtherWrite mode -> StickyOtherWritable
          | isOtherWriteMode mode -> OtherWritable
          | isStickyMode mode -> Sticky
          | otherwise -> Directory
  | Files.isSymbolicLink status = SymbolicLink
  | Files.isNamedPipe status = NamedPipe
  | Files.isSocket status = Socket
  | Files.isBlockDevice status = BlockDevise
  | Files.isCharacterDevice status = CharDevise
  | otherwise = Orphan
  where
    mode = Files.fileMode status

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

isDirectory :: NodeType -> Bool
isDirectory = \case
  StickyOtherWritable -> True
  OtherWritable -> True
  Sticky -> True
  Directory -> True
  _ -> False

data ProxyFileStatus = ProxyFileStatus
  { pfsFileMode :: Types.FileMode,
    pfsFileID :: Types.FileID,
    pfsLinkCount :: Types.LinkCount,
    pfsUserID :: Types.UserID,
    pfsGroupID :: Types.GroupID,
    pfsFileSize :: Types.FileOffset,
    pfsModificationTime :: POSIXTime,
    pfsAccessTime :: POSIXTime,
    pfsStatusChangeTime :: POSIXTime,
    pfsNodeType :: NodeType
  }

mkProxyFileStatus :: Files.FileStatus -> ProxyFileStatus
mkProxyFileStatus status =
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
      pfsNodeType = mkNodeType status
    }

data NodeInfo = NodeInfo
  { getNodePath :: FilePath,
    getNodeStatus :: ProxyFileStatus,
    getNodeContext :: T.Text,
    getNodeDirName :: FilePath,
    getNodeLinkInfo :: Maybe (Either OrphanedLinkNodeInfo LinkNodeInfo),
    getTreeNodePositions :: [Tree.TreeNodePosition]
  }

-- | Create a filenode infomation from a filepath.
mkNodeInfo :: (MonadCatch m, MonadIO m) => Option.Option -> FilePath -> FilePath -> m NodeInfo
mkNodeInfo opt dirname basename = do
  status <- Utils.getSymbolicLinkStatus path
  context <- fileContext path

  if Files.isSymbolicLink status
    then do
      linkPath <- tryIO $ Utils.readSymbolicLink path

      destStatus <- do
        case linkPath of
          -- Dereference file status if a status presents symbolic link.
          Right p -> tryIO (Utils.destFileStatusRecursive path p) <&> eitherToMaybe
          _ -> pure Nothing

      destContext <- do
        case linkPath of
          Right p -> fileContext p
          _ -> pure defaultContext

      return $ case (linkPath, destStatus) of
        (Right p, Nothing) ->
          NodeInfo
            { getNodePath = basename,
              getNodeStatus = mkProxyFileStatus status,
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
                  getNodeStatus = mkProxyFileStatus s,
                  getNodeContext = T.pack context,
                  getNodeDirName = dirname,
                  getNodeLinkInfo = Nothing,
                  getTreeNodePositions = []
                }
          | otherwise ->
              NodeInfo
                { getNodePath = basename,
                  getNodeStatus = mkProxyFileStatus status,
                  getNodeContext = T.pack context,
                  getNodeDirName = dirname,
                  getNodeLinkInfo =
                    Just . Right $
                      LinkNodeInfo
                        { getLinkNodePath = p,
                          getLinkNodeStatus = mkProxyFileStatus s,
                          getLinkNodeContext = T.pack destContext
                        },
                  getTreeNodePositions = []
                }
        _ ->
          NodeInfo
            { getNodePath = basename,
              getNodeStatus = mkProxyFileStatus status,
              getNodeContext = T.pack context,
              getNodeDirName = dirname,
              getNodeLinkInfo = Nothing,
              getTreeNodePositions = []
            }
    else
      return $
        NodeInfo
          { getNodePath = basename,
            getNodeStatus = mkProxyFileStatus status,
            getNodeContext = T.pack context,
            getNodeDirName = dirname,
            getNodeLinkInfo = Nothing,
            getTreeNodePositions = []
          }
  where
    path = dirname </> basename

fileMode :: NodeInfo -> Types.FileMode
fileMode = pfsFileMode . getNodeStatus

fileID :: NodeInfo -> Types.FileID
fileID = pfsFileID . getNodeStatus

linkCount :: NodeInfo -> Types.LinkCount
linkCount = pfsLinkCount . getNodeStatus

userID :: NodeInfo -> Types.UserID
userID = pfsUserID . getNodeStatus

groupID :: NodeInfo -> Types.GroupID
groupID = pfsGroupID . getNodeStatus

fileSize :: NodeInfo -> Types.FileOffset
fileSize = pfsFileSize . getNodeStatus

modificationTime :: NodeInfo -> POSIXTime
modificationTime = pfsModificationTime . getNodeStatus

accessTime :: NodeInfo -> POSIXTime
accessTime = pfsAccessTime . getNodeStatus

changeTime :: NodeInfo -> POSIXTime
changeTime = pfsStatusChangeTime . getNodeStatus

nodeType :: NodeInfo -> NodeType
nodeType = pfsNodeType . getNodeStatus

data LinkNodeInfo = LinkNodeInfo
  { getLinkNodePath :: FilePath,
    getLinkNodeStatus :: ProxyFileStatus,
    getLinkNodeContext :: T.Text
  }

newtype OrphanedLinkNodeInfo = OrphanedLinkNodeInfo {getOrphanedNodeLinkPath :: FilePath}

-- | Convert a 'NodeInfo' value to the dereferenced 'NodeInfo'.
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
    pure $ fromRight defaultContext context
#else
fileContext _ = pure defaultContext
#endif
{- ORMOLU_ENABLE -}

defaultContext :: String
defaultContext = "?"
