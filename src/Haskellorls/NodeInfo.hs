module Haskellorls.NodeInfo
  ( NodeInfo (..),
    nodeInfo,
    nodeInfoStatus,
    nodeInfoPath,
    toFileInfo,
  )
where

import qualified Data.List as L
import qualified Haskellorls.Utils as Utils
import qualified System.FilePath.Posix as Posix
import qualified System.Posix.Files as Files

data NodeInfo
  = FileInfo
      { getFilePath :: FilePath,
        getFileStatus :: Files.FileStatus
      }
  | LinkInfo
      { getLinkPath :: FilePath,
        getLinkStatus :: Files.FileStatus,
        getDestPath :: FilePath,
        getDestStatus :: Files.FileStatus
      }
  | OrphanedLinkInfo
      { getOrphanedLinkPath :: FilePath,
        getOrphanedLinkStatus :: Files.FileStatus,
        getDestPath :: FilePath
      }

nodeInfo :: FilePath -> FilePath -> IO NodeInfo
nodeInfo dirname basename = do
  status <- Files.getSymbolicLinkStatus path
  if Files.isSymbolicLink status
    then do
      linkPath <- Files.readSymbolicLink path
      let linkAbsPath = linkDestPath path linkPath
      isLinked <- Utils.linked linkAbsPath
      if isLinked
        then do
          destStatus <- linkDestStatus path linkPath
          return $
            LinkInfo
              { getLinkPath = basename,
                getLinkStatus = status,
                getDestPath = linkPath,
                getDestStatus = destStatus
              }
        else
          return $
            OrphanedLinkInfo
              { getOrphanedLinkPath = basename,
                getOrphanedLinkStatus = status,
                getDestPath = linkPath
              }
    else
      return $
        FileInfo
          { getFilePath = basename,
            getFileStatus = status
          }
  where
    path = dirname Posix.</> basename

toFileInfo :: NodeInfo -> NodeInfo
toFileInfo node = case node of
  FileInfo {} -> node
  LinkInfo {} ->
    FileInfo
      { getFilePath = getDestPath node,
        getFileStatus = getDestStatus node
      }
  OrphanedLinkInfo {} ->
    FileInfo
      { getFilePath = getDestPath node,
        getFileStatus = getOrphanedLinkStatus node
      }

linkDestPath :: FilePath -> FilePath -> FilePath
linkDestPath parPath linkPath
  | isAbsPath linkPath = linkPath
  | otherwise = Posix.takeDirectory parPath Posix.</> linkPath

isAbsPath :: FilePath -> Bool
isAbsPath path = "/" `L.isPrefixOf` path

linkDestStatus :: FilePath -> FilePath -> IO Files.FileStatus
linkDestStatus parPath = Files.getFileStatus . linkDestPath parPath

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
