-- | "System.Posix.Files.ByteString" with extras.
module Haskellorls.System.Posix.Files.ByteString
  ( destFileStatusRecursive,
    module System.Posix.Files.ByteString,
  )
where

import Control.Monad.IO.Class
import System.FilePath.Posix.ByteString
import System.Posix.Files.ByteString

-- | A useful version of 'getFileStatus', to get a resolved destination file's
-- status, but not a symbolic link self.
destFileStatusRecursive :: MonadIO m => RawFilePath -> RawFilePath -> m FileStatus
destFileStatusRecursive dirPath basePath = do
  s <- liftIO . getFileStatus $ linkDestPath dirPath basePath

  if isSymbolicLink s
    then do
      link <- liftIO . readSymbolicLink $ linkDestPath dirPath basePath
      destFileStatusRecursive dirPath link
    else pure s

-- | Create a symlink's filepath from symlink's parent directory path and a
-- filepath written in symlink self.
linkDestPath :: RawFilePath -> RawFilePath -> RawFilePath
linkDestPath parPath linkPath
  | isAbsolute linkPath = linkPath
  | otherwise = takeDirectory parPath </> linkPath
