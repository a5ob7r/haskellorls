-- | "System.Posix.Files.ByteString" with extras.
module Haskellorls.System.Posix.Files.ByteString
  ( destFileStatusRecursive,
    major,
    minor,
    module System.Posix.Files.ByteString,
  )
where

import Control.Monad.IO.Class
import Data.Bits
import System.FilePath.Posix.ByteString
import System.Posix.Files.ByteString
import System.Posix.Types (DeviceID)

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

-- | A major number of a device.
major :: DeviceID -> DeviceID
major did = (did .&. 0x00000000000fff00) `shiftR` 8 .|. (did .&. 0xfffff00000000000) `shiftR` 32

-- | A minor number of a device.
minor :: DeviceID -> DeviceID
minor did = (did .&. 0x00000000000000ff) `shiftR` 0 .|. (did .&. 0x00000ffffff00000) `shiftR` 12
