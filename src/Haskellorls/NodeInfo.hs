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
      linkPath <- Files.readSymbolicLink path
      let linkAbsPath = linkDestPath path linkPath
      destStatus <- Utils.destFileStatus linkAbsPath
      return $ case destStatus of
        Nothing ->
          OrphanedLinkInfo
            { getOrphanedLinkPath = basename,
              getOrphanedLinkStatus = status,
              getDestPath = linkPath,
              getTreeNodePositions = []
            }
        Just s
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
                getDestPath = linkPath,
                getDestStatus = s,
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
toFileInfo node = case node of
  FileInfo {} -> node
  LinkInfo {} ->
    FileInfo
      { getFilePath = getDestPath node,
        getFileStatus = getDestStatus node,
        getTreeNodePositions = getTreeNodePositions node
      }
  OrphanedLinkInfo {} ->
    FileInfo
      { getFilePath = getDestPath node,
        getFileStatus = getOrphanedLinkStatus node,
        getTreeNodePositions = getTreeNodePositions node
      }

linkDestPath :: FilePath -> FilePath -> FilePath
linkDestPath parPath linkPath
  | isAbsPath linkPath = linkPath
  | otherwise = Posix.takeDirectory parPath Posix.</> linkPath

isAbsPath :: FilePath -> Bool
isAbsPath path = "/" `L.isPrefixOf` path

nodeInfoStatus :: NodeInfo -> Files.FileStatus
nodeInfoStatus node = nodeStatus node
  where
    nodeStatus = case node of
      FileInfo {} -> getFileStatus
      LinkInfo {} -> getLinkStatus
      OrphanedLinkInfo {} -> getOrphanedLinkStatus

nodeInfoPath :: NodeInfo -> FilePath
nodeInfoPath node = pathFunc node
  where
    pathFunc = case node of
      FileInfo {} -> getFilePath
      LinkInfo {} -> getLinkPath
      OrphanedLinkInfo {} -> getOrphanedLinkPath
