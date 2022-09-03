module Haskellorls.NodeInfo
  ( NodeType (..),
    NodeInfo (..),
    fileID,
    fileMode,
    linkCount,
    fileSize,
    userID,
    groupID,
    fileTime,
    specialDeviceID,
    nodeType,
    isDirectory,
    mkNodeInfo,
    toFileInfo,
  )
where

import Control.Exception.Safe (MonadCatch, tryIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Time as Time
import qualified Haskellorls.Config.Tree as Tree
import qualified Haskellorls.System.Posix.Files.ByteString as Files
import System.FilePath.Posix.ByteString (RawFilePath, (</>))
import qualified System.Posix.Types as Types

#ifdef SELINUX
import Data.Either (fromRight)
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
          | mode `Files.hasFileMode` Files.setUserIDMode -> Setuid
          | mode `Files.hasFileMode` Files.setGroupIDMode -> Setgid
          | mode `Files.hasFileModesOr` [Files.ownerExecuteMode, Files.groupExecuteMode, Files.otherExecuteMode] -> Executable
          | otherwise -> File
  | Files.isDirectory status = case (mode `Files.hasFileMode` Files.otherWriteMode, mode `Files.hasFileMode` Files.stickyMode) of
      (True, True) -> StickyOtherWritable
      (True, _) -> OtherWritable
      (_, True) -> Sticky
      _ -> Directory
  | Files.isSymbolicLink status = SymbolicLink
  | Files.isNamedPipe status = NamedPipe
  | Files.isSocket status = Socket
  | Files.isBlockDevice status = BlockDevise
  | Files.isCharacterDevice status = CharDevise
  | otherwise = Orphan
  where
    mode = Files.fileMode status

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
    pfsFileTime :: POSIXTime,
    pfsSpecialDeviceID :: Types.DeviceID,
    pfsNodeType :: NodeType
  }

mkProxyFileStatus :: Config.Config -> Files.FileStatus -> ProxyFileStatus
mkProxyFileStatus config status =
  ProxyFileStatus
    { pfsFileMode = Files.fileMode status,
      pfsFileID = Files.fileID status,
      pfsLinkCount = Files.linkCount status,
      pfsUserID = Files.fileOwner status,
      pfsGroupID = Files.fileGroup status,
      pfsFileSize = Files.fileSize status,
      pfsFileTime = case Config.time config of
        Time.MODIFICATION -> Files.modificationTimeHiRes status
        Time.ACCESS -> Files.accessTimeHiRes status
        Time.CHANGE -> Files.statusChangeTimeHiRes status,
      pfsSpecialDeviceID = Files.specialDeviceID status,
      pfsNodeType = mkNodeType status
    }

data NodeInfo = NodeInfo
  { getNodePath :: RawFilePath,
    getNodeStatus :: ProxyFileStatus,
    getNodeContext :: T.Text,
    getNodeDirName :: RawFilePath,
    getNodeLinkInfo :: Maybe (Either OrphanedLinkNodeInfo LinkNodeInfo),
    -- | This contains reversed list due to the performance problem.
    getTreeNodePositions :: [Tree.TreeNodePosition]
  }

-- | Create a filenode infomation from a filepath.
mkNodeInfo :: (MonadCatch m, MonadIO m) => Config.Config -> RawFilePath -> RawFilePath -> m NodeInfo
mkNodeInfo config dirname basename = do
  let filepath = dirname </> basename
  filestatus <- liftIO $ Files.getSymbolicLinkStatus filepath
  filecontext <- fileContext filepath

  if Files.isSymbolicLink filestatus
    then do
      linkpath <- tryIO . liftIO $ Files.readSymbolicLink filepath

      linkstatus <- case linkpath of
        Right _ -> eitherToMaybe <$> tryIO (liftIO $ Files.getFileStatus filepath)
        _ -> return Nothing

      linkcontext <- case linkpath of
        Right path -> fileContext path
        _ -> return defaultContext

      return $ case (linkpath, linkstatus) of
        (Right p, Nothing) ->
          NodeInfo
            { getNodePath = basename,
              getNodeStatus = mkProxyFileStatus config filestatus,
              getNodeContext = T.pack filecontext,
              getNodeDirName = dirname,
              getNodeLinkInfo = Just . Left $ OrphanedLinkNodeInfo p,
              getTreeNodePositions = []
            }
        (Right p, Just s)
          | Config.dereference config
              || Config.dereferenceCommandLine config
              || Config.dereferenceCommandLineSymlinkToDir config && Files.isDirectory s ->
              NodeInfo
                { getNodePath = basename,
                  getNodeStatus = mkProxyFileStatus config s,
                  getNodeContext = T.pack filecontext,
                  getNodeDirName = dirname,
                  getNodeLinkInfo = Nothing,
                  getTreeNodePositions = []
                }
          | otherwise ->
              NodeInfo
                { getNodePath = basename,
                  getNodeStatus = mkProxyFileStatus config filestatus,
                  getNodeContext = T.pack filecontext,
                  getNodeDirName = dirname,
                  getNodeLinkInfo =
                    Just . Right $
                      LinkNodeInfo
                        { getLinkNodePath = p,
                          getLinkNodeStatus = mkProxyFileStatus config s,
                          getLinkNodeContext = T.pack linkcontext
                        },
                  getTreeNodePositions = []
                }
        _ ->
          NodeInfo
            { getNodePath = basename,
              getNodeStatus = mkProxyFileStatus config filestatus,
              getNodeContext = T.pack filecontext,
              getNodeDirName = dirname,
              getNodeLinkInfo = Nothing,
              getTreeNodePositions = []
            }
    else
      return $
        NodeInfo
          { getNodePath = basename,
            getNodeStatus = mkProxyFileStatus config filestatus,
            getNodeContext = T.pack filecontext,
            getNodeDirName = dirname,
            getNodeLinkInfo = Nothing,
            getTreeNodePositions = []
          }

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

fileTime :: NodeInfo -> POSIXTime
fileTime = pfsFileTime . getNodeStatus

specialDeviceID :: NodeInfo -> Types.DeviceID
specialDeviceID = pfsSpecialDeviceID . getNodeStatus

nodeType :: NodeInfo -> NodeType
nodeType = pfsNodeType . getNodeStatus

data LinkNodeInfo = LinkNodeInfo
  { getLinkNodePath :: RawFilePath,
    getLinkNodeStatus :: ProxyFileStatus,
    getLinkNodeContext :: T.Text
  }

newtype OrphanedLinkNodeInfo = OrphanedLinkNodeInfo {getOrphanedNodeLinkPath :: RawFilePath}

-- | Convert a 'NodeInfo' value to the dereferenced 'NodeInfo'.
toFileInfo :: NodeInfo -> NodeInfo
toFileInfo node = case getNodeLinkInfo node of
  Nothing -> node
  Just (Right LinkNodeInfo {..}) ->
    node
      { getNodePath = getLinkNodePath,
        getNodeStatus = getLinkNodeStatus,
        getNodeContext = getLinkNodeContext
      }
  Just (Left OrphanedLinkNodeInfo {..}) ->
    node
      { getNodePath = getOrphanedNodeLinkPath,
        getNodeStatus = (getNodeStatus node) {pfsNodeType = File}
      }

{- ORMOLU_DISABLE -}
-- |
-- NOTE: This is not tested on SELinux enabled environment, so this may be
-- broken.
-- NOTE: Disable ormolu because it does not support CPP extension.
fileContext :: MonadIO m => RawFilePath -> m String
#ifdef SELINUX
fileContext path = fromRight defaultContext <$> tryIO (SELinux.getFileCon path)
#else
fileContext _ = return defaultContext
#endif
{- ORMOLU_ENABLE -}

defaultContext :: String
defaultContext = "?"
