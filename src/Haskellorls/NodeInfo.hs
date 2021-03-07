{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.NodeInfo
  ( NodeInfo (..),
    nodeInfo,
    nodeInfoStatus,
    nodeInfoPath,
    nodeInfoContext,
    toFileInfo,
  )
where

import qualified Control.Exception.Base as Exception
import qualified Data.Either as Either
import qualified Data.List as L
import qualified Data.Text as T
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Tree.Type as Tree
import qualified Haskellorls.Utils as Utils
import qualified System.FilePath.Posix as Posix
import qualified System.IO as IO
import qualified System.Posix.Files as Files
#ifdef SELINUX
import qualified System.Linux.SELinux as SELinux
#endif

data NodeInfo
  = FileInfo
      { getFilePath :: FilePath,
        getFileStatus :: Files.FileStatus,
        getFileContext :: T.Text,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }
  | LinkInfo
      { getLinkPath :: FilePath,
        getLinkStatus :: Files.FileStatus,
        getLinkContext :: T.Text,
        getDestPath :: FilePath,
        getDestStatus :: Files.FileStatus,
        getDestContext :: T.Text,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }
  | OrphanedLinkInfo
      { getOrphanedLinkPath :: FilePath,
        getOrphanedLinkStatus :: Files.FileStatus,
        getOrphanedLinkContext :: T.Text,
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
              getOrphanedLinkStatus = status,
              getOrphanedLinkContext = T.pack context,
              getDestPath = p,
              getTreeNodePositions = []
            }
        (Right p, Just s)
          | Option.dereference opt
              || Option.dereferenceCommandLine opt
              || (Option.dereferenceCommandLineSymlinkToDir opt && Files.isDirectory s) ->
            FileInfo
              { getFilePath = basename,
                getFileStatus = s,
                getFileContext = T.pack context,
                getTreeNodePositions = []
              }
          | otherwise ->
            LinkInfo
              { getLinkPath = basename,
                getLinkStatus = status,
                getLinkContext = T.pack context,
                getDestPath = p,
                getDestStatus = s,
                getDestContext = T.pack destContext,
                getTreeNodePositions = []
              }
        _ ->
          FileInfo
            { getFilePath = basename,
              getFileStatus = status,
              getFileContext = T.pack context,
              getTreeNodePositions = []
            }
    else
      return $
        FileInfo
          { getFilePath = basename,
            getFileStatus = status,
            getFileContext = T.pack context,
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
        ..
      }
  OrphanedLinkInfo {..} ->
    FileInfo
      { getFilePath = getDestPath,
        getFileStatus = getOrphanedLinkStatus,
        getFileContext = getOrphanedLinkContext,
        ..
      }

nodeInfoStatus :: NodeInfo -> Files.FileStatus
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

-- | NOTE: This is not tested on SELinux enabled environment so maybe break.
fileContext :: FilePath -> IO String
fileContext path =
#ifdef SELINUX
  do
    context <- Exception.try (SELinux.getFileCon path) :: IO (Either Exception.IOException String)
    pure $ Either.fromRight defaultContext context
#else
  pure defaultContext
#endif

defaultContext :: String
defaultContext = "?"
