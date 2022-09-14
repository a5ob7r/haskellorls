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
    allocSize,
    blockSize,
    specialDeviceID,
    nodeType,
    isDirectory,
    mkNodeInfo,
    dereference,
    dereferencedNodePath,
  )
where

import Control.Exception.Safe (MonadCatch, tryIO)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.TimeType as TimeType
import qualified Haskellorls.Config.Tree as Tree
import Haskellorls.System.OsPath.Posix.Extra (PosixPath, (</>))
import qualified Haskellorls.System.Posix.PosixString as Posix

#ifdef SELINUX
import Data.Either (fromRight)
import Haskellorls.System.OsPath.Posix.Extra (decode)
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

mkNodeType :: Posix.FileStatus -> NodeType
mkNodeType status
  | Posix.isRegularFile status =
      if
          | mode `Posix.hasFileMode` Posix.setUserIDMode -> Setuid
          | mode `Posix.hasFileMode` Posix.setGroupIDMode -> Setgid
          | mode `Posix.hasFileModesOr` [Posix.ownerExecuteMode, Posix.groupExecuteMode, Posix.otherExecuteMode] -> Executable
          | otherwise -> File
  | Posix.isDirectory status = case (mode `Posix.hasFileMode` Posix.otherWriteMode, mode `Posix.hasFileMode` Posix.stickyMode) of
      (True, True) -> StickyOtherWritable
      (True, _) -> OtherWritable
      (_, True) -> Sticky
      _ -> Directory
  | Posix.isSymbolicLink status = SymbolicLink
  | Posix.isNamedPipe status = NamedPipe
  | Posix.isSocket status = Socket
  | Posix.isBlockDevice status = BlockDevise
  | Posix.isCharacterDevice status = CharDevise
  | otherwise = Orphan
  where
    mode = Posix.fileMode status

isDirectory :: NodeType -> Bool
isDirectory = \case
  StickyOtherWritable -> True
  OtherWritable -> True
  Sticky -> True
  Directory -> True
  _ -> False

data ProxyFileStatus = ProxyFileStatus
  { pfsFileMode :: Posix.FileMode,
    pfsFileID :: Posix.FileID,
    pfsLinkCount :: Posix.LinkCount,
    pfsUserID :: Posix.UserID,
    pfsGroupID :: Posix.GroupID,
    pfsFileSize :: Posix.FileOffset,
    pfsAllocSize :: Maybe Posix.FileOffset,
    pfsBlockSize :: Maybe Posix.FileOffset,
    pfsFileTime :: POSIXTime,
    pfsSpecialDeviceID :: Posix.DeviceID,
    pfsNodeType :: NodeType
  }

mkProxyFileStatus :: Config.Config -> Posix.FileStatus -> ProxyFileStatus
mkProxyFileStatus config status =
  ProxyFileStatus
    { pfsFileMode = Posix.fileMode status,
      pfsFileID = Posix.fileID status,
      pfsLinkCount = Posix.linkCount status,
      pfsUserID = Posix.fileOwner status,
      pfsGroupID = Posix.fileGroup status,
      pfsFileSize = Posix.fileSize status,
      pfsAllocSize = (\n -> fromIntegral n * 512) <$> Posix.fileBlocks status,
      pfsBlockSize = fromIntegral <$> Posix.fileBlockSize status,
      pfsFileTime = case Config.time config of
        TimeType.MODIFICATION -> Posix.modificationTimeHiRes status
        TimeType.ACCESS -> Posix.accessTimeHiRes status
        TimeType.CHANGE -> Posix.statusChangeTimeHiRes status,
      pfsSpecialDeviceID = Posix.specialDeviceID status,
      pfsNodeType = mkNodeType status
    }

data NodeInfo = NodeInfo
  { getNodePath :: PosixPath,
    getNodeStatus :: Maybe ProxyFileStatus,
    getNodeContext :: T.Text,
    getNodeDirName :: PosixPath,
    getNodeLinkInfo :: Maybe (Either OrphanedLinkNodeInfo LinkNodeInfo),
    -- | This contains reversed list due to the performance problem.
    getTreeNodePositions :: [Tree.TreeNodePosition]
  }

-- | Create a filenode infomation from a filepath.
mkNodeInfo :: (MonadCatch m, MonadIO m) => Config.Config -> PosixPath -> PosixPath -> m NodeInfo
mkNodeInfo config dirname basename = do
  let getNodePath = basename
      getNodeDirName = dirname
      getTreeNodePositions = []
      filepath = dirname </> basename

  getNodeContext <- T.pack <$> fileContext filepath

  filestatus <- liftIO $ Posix.getSymbolicLinkStatus filepath

  node <-
    if Posix.isSymbolicLink filestatus
      then do
        linkpath <- tryIO . liftIO $ Posix.readSymbolicLink filepath

        linkstatus <- case linkpath of
          Right _ -> eitherToMaybe <$> tryIO (liftIO $ Posix.getFileStatus filepath)
          _ -> return Nothing

        linkcontext <- case linkpath of
          Right path -> fileContext path
          _ -> return defaultContext

        return $ case (linkpath, linkstatus) of
          (Right p, Nothing) ->
            let getNodeStatus = Just $ mkProxyFileStatus config filestatus
                getNodeLinkInfo = Just . Left $ OrphanedLinkNodeInfo p
             in NodeInfo {..}
          (Right p, Just s) ->
            let getNodeStatus = Just $ mkProxyFileStatus config filestatus
                getLinkNodePath = p
                getLinkNodeStatus = mkProxyFileStatus config s
                getLinkNodeContext = T.pack linkcontext
                getNodeLinkInfo = Just . Right $ LinkNodeInfo {..}
             in NodeInfo {..}
          _ ->
            let getNodeStatus = Just $ mkProxyFileStatus config filestatus
                getNodeLinkInfo = Nothing
             in NodeInfo {..}
      else
        let getNodeStatus = Just $ mkProxyFileStatus config filestatus
            getNodeLinkInfo = Nothing
         in return NodeInfo {..}

  return
    if Config.dereference config
      || Config.dereferenceCommandLine config
      || Config.dereferenceCommandLineSymlinkToDir config
        && maybe False (either (const False) (isDirectory . pfsNodeType . getLinkNodeStatus)) (getNodeLinkInfo node)
      then dereference node
      else node

fileMode :: NodeInfo -> Maybe Posix.FileMode
fileMode = fmap pfsFileMode . getNodeStatus

fileID :: NodeInfo -> Maybe Posix.FileID
fileID = fmap pfsFileID . getNodeStatus

linkCount :: NodeInfo -> Maybe Posix.LinkCount
linkCount = fmap pfsLinkCount . getNodeStatus

userID :: NodeInfo -> Maybe Posix.UserID
userID = fmap pfsUserID . getNodeStatus

groupID :: NodeInfo -> Maybe Posix.GroupID
groupID = fmap pfsGroupID . getNodeStatus

fileSize :: NodeInfo -> Maybe Posix.FileOffset
fileSize = fmap pfsFileSize . getNodeStatus

fileTime :: NodeInfo -> Maybe POSIXTime
fileTime = fmap pfsFileTime . getNodeStatus

specialDeviceID :: NodeInfo -> Maybe Posix.DeviceID
specialDeviceID = fmap pfsSpecialDeviceID . getNodeStatus

allocSize :: NodeInfo -> Maybe Posix.FileOffset
allocSize = getNodeStatus >=> pfsAllocSize

blockSize :: NodeInfo -> Maybe Posix.FileOffset
blockSize = getNodeStatus >=> pfsBlockSize

nodeType :: NodeInfo -> Maybe NodeType
nodeType = fmap pfsNodeType . getNodeStatus

data LinkNodeInfo = LinkNodeInfo
  { getLinkNodePath :: PosixPath,
    getLinkNodeStatus :: ProxyFileStatus,
    getLinkNodeContext :: T.Text
  }

newtype OrphanedLinkNodeInfo = OrphanedLinkNodeInfo {getOrphanedNodeLinkPath :: PosixPath}

-- | Convert a 'NodeInfo' value to the dereferenced 'NodeInfo'.
dereference :: NodeInfo -> NodeInfo
dereference node = case getNodeLinkInfo node of
  Nothing -> node
  Just (Right LinkNodeInfo {..}) ->
    node
      { getNodeStatus = Just getLinkNodeStatus,
        getNodeContext = getLinkNodeContext,
        getNodeLinkInfo = Nothing
      }
  Just (Left OrphanedLinkNodeInfo {}) ->
    node
      { getNodeStatus = Nothing,
        getNodeLinkInfo = Nothing
      }

-- | Get a dereferenced node's filepath.
dereferencedNodePath :: Either OrphanedLinkNodeInfo LinkNodeInfo -> PosixPath
dereferencedNodePath = either getOrphanedNodeLinkPath getLinkNodePath

{- ORMOLU_DISABLE -}
-- |
-- NOTE: This is not tested on SELinux enabled environment, so this may be
-- broken.
-- NOTE: Disable ormolu because it does not support CPP extension.
fileContext :: MonadIO m => PosixPath -> m String
#ifdef SELINUX
fileContext path = fromRight defaultContext <$> tryIO (SELinux.getFileCon $ decode path)
#else
fileContext _ = return defaultContext
#endif
{- ORMOLU_ENABLE -}

defaultContext :: String
defaultContext = "?"
