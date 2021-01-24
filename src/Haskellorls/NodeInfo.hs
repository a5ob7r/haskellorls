module Haskellorls.NodeInfo
  ( NodeInfo (..),
    nodeInfo,
    nodeInfoStatus,
    nodeInfoPath,
  )
where

import qualified Data.List as L
import qualified System.FilePath.Posix as Posix (takeDirectory, (</>))
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

nodeInfo :: FilePath -> IO NodeInfo
nodeInfo path = do
  status <- Files.getSymbolicLinkStatus path
  if Files.isSymbolicLink status
    then do
      linkPath <- Files.readSymbolicLink path
      let linkAbsPath = linkDestPath path linkPath
      isLinked <- Files.fileExist linkAbsPath
      if isLinked
        then do
          destStatus <- linkDestStatus path linkPath
          return $
            LinkInfo
              { getLinkPath = path,
                getLinkStatus = status,
                getDestPath = linkPath,
                getDestStatus = destStatus
              }
        else
          return $
            OrphanedLinkInfo
              { getOrphanedLinkPath = path,
                getOrphanedLinkStatus = status,
                getDestPath = path
              }
    else
      return $
        FileInfo
          { getFilePath = path,
            getFileStatus = status
          }

linkDestPath :: FilePath -> FilePath -> FilePath
linkDestPath parPath linkPath =
  if "/" `L.isPrefixOf` linkPath
    then linkPath
    else Posix.takeDirectory parPath Posix.</> linkPath

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
