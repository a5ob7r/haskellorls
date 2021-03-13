{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.NodeInfo
  ( NodeType (..),
    NodeInfo (..),
    ProxyFileStatus (..),
    isDirectory,
    nodeInfo,
    nodeInfoStatus,
    nodeInfoPath,
    nodeInfoContext,
    nodeInfoDirName,
    toFileInfo,
  )
where

import qualified Control.Exception.Base as Exception
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Clock
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Tree.Type as Tree
import qualified Haskellorls.Utils as Utils
import qualified System.FilePath.Posix as Posix
import qualified System.IO as IO
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

#ifdef SELINUX
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

data NodeInfo
  = FileInfo
      { getFilePath :: FilePath,
        getFileStatus :: ProxyFileStatus,
        getFileContext :: T.Text,
        getFileDirName :: FilePath,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }
  | LinkInfo
      { getLinkPath :: FilePath,
        getLinkStatus :: ProxyFileStatus,
        getLinkContext :: T.Text,
        getLinkDirName :: FilePath,
        getDestPath :: FilePath,
        getDestStatus :: ProxyFileStatus,
        getDestContext :: T.Text,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }
  | OrphanedLinkInfo
      { getOrphanedLinkPath :: FilePath,
        getOrphanedLinkStatus :: ProxyFileStatus,
        getOrphanedLinkContext :: T.Text,
        getOrphanedLinkDirName :: FilePath,
        getDestPath :: FilePath,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }

nodeInfo :: Option.Option -> FilePath -> FilePath -> IO NodeInfo
nodeInfo opt dirname basename = do
  status <- Files.getSymbolicLinkStatus path
  context <- fileContext path
  if Files.isSymbolicLink status
    then do
      linkPath <- Utils.readSymbolicLink path
      case linkPath of
        Left e -> IO.hPrint IO.stderr e
        _ -> pure ()

      destStatus <- do
        case linkPath of
          -- Dereference file status if a status presents symbolic link.
          Right p -> Utils.destFileStatusRecursive path p
          _ -> pure Nothing

      destContext <- do
        case linkPath of
          Right p -> fileContext p
          _ -> pure defaultContext

      return $ case (linkPath, destStatus) of
        (Right p, Nothing) ->
          OrphanedLinkInfo
            { getOrphanedLinkPath = basename,
              getOrphanedLinkStatus = proxyFileStatus status,
              getOrphanedLinkContext = T.pack context,
              getOrphanedLinkDirName = dirname,
              getDestPath = p,
              getTreeNodePositions = []
            }
        (Right p, Just s)
          | Option.dereference opt
              || Option.dereferenceCommandLine opt
              || (Option.dereferenceCommandLineSymlinkToDir opt && Files.isDirectory s) ->
            FileInfo
              { getFilePath = basename,
                getFileStatus = proxyFileStatus s,
                getFileContext = T.pack context,
                getFileDirName = dirname,
                getTreeNodePositions = []
              }
          | otherwise ->
            LinkInfo
              { getLinkPath = basename,
                getLinkStatus = proxyFileStatus status,
                getLinkContext = T.pack context,
                getLinkDirName = dirname,
                getDestPath = p,
                getDestStatus = proxyFileStatus s,
                getDestContext = T.pack destContext,
                getTreeNodePositions = []
              }
        _ ->
          FileInfo
            { getFilePath = basename,
              getFileStatus = proxyFileStatus status,
              getFileContext = T.pack context,
              getFileDirName = dirname,
              getTreeNodePositions = []
            }
    else
      return $
        FileInfo
          { getFilePath = basename,
            getFileStatus = proxyFileStatus status,
            getFileContext = T.pack context,
            getFileDirName = dirname,
            getTreeNodePositions = []
          }
  where
    path = dirname Posix.</> basename

toFileInfo :: NodeInfo -> NodeInfo
toFileInfo = \case
  node@FileInfo {} -> node
  LinkInfo {..} ->
    FileInfo
      { getFilePath = getDestPath,
        getFileStatus = getDestStatus,
        getFileContext = getDestContext,
        getFileDirName = getLinkDirName,
        ..
      }
  OrphanedLinkInfo {..} ->
    FileInfo
      { getFilePath = getDestPath,
        getFileStatus = getOrphanedLinkStatus,
        getFileContext = getOrphanedLinkContext,
        getFileDirName = getOrphanedLinkDirName,
        ..
      }

nodeInfoStatus :: NodeInfo -> ProxyFileStatus
nodeInfoStatus = \case
  FileInfo {..} -> getFileStatus
  LinkInfo {..} -> getLinkStatus
  OrphanedLinkInfo {..} -> getOrphanedLinkStatus

nodeInfoPath :: NodeInfo -> FilePath
nodeInfoPath = \case
  FileInfo {..} -> getFilePath
  LinkInfo {..} -> getLinkPath
  OrphanedLinkInfo {..} -> getOrphanedLinkPath

nodeInfoContext :: NodeInfo -> T.Text
nodeInfoContext = \case
  FileInfo {..} -> getFileContext
  LinkInfo {..} -> getLinkContext
  OrphanedLinkInfo {..} -> getOrphanedLinkContext

nodeInfoDirName :: NodeInfo -> FilePath
nodeInfoDirName = \case
  FileInfo {..} -> getFileDirName
  LinkInfo {..} -> getLinkDirName
  OrphanedLinkInfo {..} -> getOrphanedLinkDirName

{- ORMOLU_DISABLE -}
-- |
-- NOTE: This is not tested on SELinux enabled environment so maybe break.
-- NOTE: Disable ormolu because it does not support CPP extension.
fileContext :: FilePath -> IO String
fileContext path =
#ifdef SELINUX
  do
    context <- Exception.try (SELinux.getFileCon path) :: IO (Either Exception.IOException String)
    pure $ Either.fromRight defaultContext context
#else
  pure defaultContext
#endif
{- ORMOLU_ENABLE -}

defaultContext :: String
defaultContext = "?"
