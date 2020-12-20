module Haskellorls.NodeInfo
  ( NodeInfo(..)
  , nodeInfo
  ) where

import qualified System.Posix.Files as Files
    ( FileStatus
    , getSymbolicLinkStatus
    , isSymbolicLink
    , readSymbolicLink
    , fileExist
    , getFileStatus
    )

data NodeInfo
  = NoInfo
  | NodeInfo
      { nodePath :: FilePath
      , nodeStatus :: Files.FileStatus
      , destNode :: NodeInfo
      }

nodeInfo :: FilePath -> IO NodeInfo
nodeInfo path = do
  status <- Files.getSymbolicLinkStatus path
  dest <- if Files.isSymbolicLink status
             then linkNodeInfo path
             else return NoInfo
  return $ NodeInfo { nodePath = path
                    , nodeStatus = status
                    , destNode = dest
                    }

{- Return symbolic link destination node info if `path` shows symbolic link.
-}
linkNodeInfo :: FilePath -> IO NodeInfo
linkNodeInfo path = do
  linkPath <- Files.readSymbolicLink path
  isLinked <- Files.fileExist linkPath
  if isLinked
     then linkNodeInfo' linkPath
     else return NoInfo

linkNodeInfo' :: FilePath -> IO NodeInfo
linkNodeInfo' path = do
  status <- Files.getFileStatus path
  return $ NodeInfo { nodePath = path
                    , nodeStatus = status
                    , destNode = NoInfo
                    }
