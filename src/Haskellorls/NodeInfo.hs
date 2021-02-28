{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.NodeInfo
  ( NodeInfo (..),
    nodeInfo,
    nodeInfoStatus,
    nodeInfoPath,
    toFileInfo,
  )
where

import qualified Data.List as L
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Tree.Type as Tree
import qualified Haskellorls.Utils as Utils
import qualified System.FilePath.Posix as Posix
import qualified System.IO as IO
import qualified System.Posix.Files as Files

data NodeInfo
  = FileInfo
      { getFilePath :: FilePath,
        getFileStatus :: Files.FileStatus,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }
  | LinkInfo
      { getLinkPath :: FilePath,
        getLinkStatus :: Files.FileStatus,
        getDestPath :: FilePath,
        getDestStatus :: Files.FileStatus,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }
  | OrphanedLinkInfo
      { getOrphanedLinkPath :: FilePath,
        getOrphanedLinkStatus :: Files.FileStatus,
        getDestPath :: FilePath,
        getTreeNodePositions :: [Tree.TreeNodePosition]
      }

nodeInfo :: Option.Option -> FilePath -> FilePath -> IO NodeInfo
nodeInfo opt dirname basename = do
  status <- Files.getSymbolicLinkStatus path
  if Files.isSymbolicLink status
    then do
      linkPath <- Utils.readSymbolicLink path
      case linkPath of
        Left e -> IO.hPrint IO.stderr e
        _ -> pure ()

      destStatus <- do
        case linkPath of
          Right p -> Utils.destFileStatus $ linkDestPath path p
          _ -> pure Nothing

      return $ case (linkPath, destStatus) of
        (Right p, Nothing) ->
          OrphanedLinkInfo
            { getOrphanedLinkPath = basename,
              getOrphanedLinkStatus = status,
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
                getTreeNodePositions = []
              }
          | otherwise ->
            LinkInfo
              { getLinkPath = basename,
                getLinkStatus = status,
                getDestPath = p,
                getDestStatus = s,
                getTreeNodePositions = []
              }
        _ ->
          FileInfo
            { getFilePath = basename,
              getFileStatus = status,
              getTreeNodePositions = []
            }
    else
      return $
        FileInfo
          { getFilePath = basename,
            getFileStatus = status,
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
        ..
      }
  OrphanedLinkInfo {..} ->
    FileInfo
      { getFilePath = getDestPath,
        getFileStatus = getOrphanedLinkStatus,
        ..
      }

linkDestPath :: FilePath -> FilePath -> FilePath
linkDestPath parPath linkPath
  | isAbsPath linkPath = linkPath
  | otherwise = Posix.takeDirectory parPath Posix.</> linkPath

isAbsPath :: FilePath -> Bool
isAbsPath path = "/" `L.isPrefixOf` path

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
