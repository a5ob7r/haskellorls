module Haskellorls.Node where

import System.FilePath.Posix
import System.Posix.Files
import System.Posix.Types
import System.Posix.User

import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601

data Node = Node { name :: String
                 , mode :: String
                 , owner :: String
                 , group :: String
                 , size :: String
                 , mtime :: String
                 } deriving (Show)

node :: FilePath -> IO Node
node path = do
  let name = takeFileName path
  status <- getFileStatus path
  mode <- fileModeOf status
  owner <- userNameOf status
  group <- groupNameOf status
  size <- fileSizeOf status
  mtime <- modificationTimeOf status
  return $ Node { name = name
                , mode = mode
                , owner = owner
                , group = group
                , size = size
                , mtime = mtime
                }

userNameOf :: FileStatus -> IO String
userNameOf = fmap userName . getUserEntryForID . fileOwner

groupNameOf :: FileStatus -> IO String
groupNameOf = fmap groupName . getGroupEntryForID . fileGroup

fileModeOf :: FileStatus -> IO String
fileModeOf = return . allFileMode

fileSizeOf :: FileStatus -> IO String
fileSizeOf = return . fmap show fileSize

modificationTimeOf :: FileStatus -> IO String
modificationTimeOf = return . modifiedTime . modificationTime

modifiedTime :: EpochTime -> String
modifiedTime = iso8601Show . posixSecondsToUTCTime . realToFrac

allFileMode :: FileStatus -> String
allFileMode status = foldl (\l r -> l ++ r mode) "" [ownerFileMode, groupFileMode, otherFileMode]
  where
    mode = fileMode status

readableMode :: String
readableMode = "r"

writableMode :: String
writableMode = "w"

executableMode :: String
executableMode = "x"

nullMode :: String
nullMode = "-"

ownerFileMode :: FileMode -> String
ownerFileMode = classFileMode [isOwnerReadMode, isOwnerWriteMode, isOwnerExecuteMode]

groupFileMode :: FileMode -> String
groupFileMode = classFileMode [isGroupReadMode, isGroupWriteMode, isGroupExecuteMode]

otherFileMode :: FileMode -> String
otherFileMode = classFileMode [isOtherReadMode, isOtherWriteMode, isOtherExecuteMode]

classFileMode :: [(FileMode -> Bool)] -> FileMode -> String
classFileMode fs mode = foldl (\l r -> l ++ r) "" . map (\(f, perm) -> perm f mode) $ zip fs perms
  where
    perms = [readablePerm, writablePerm, executablePerm]

readablePerm :: (FileMode -> Bool) -> FileMode -> String
readablePerm f mode = if f mode
                         then readableMode
                         else nullMode

writablePerm :: (FileMode -> Bool) -> FileMode -> String
writablePerm f mode = if f mode
                         then writableMode
                         else nullMode

executablePerm :: (FileMode -> Bool) -> FileMode -> String
executablePerm f mode = if f mode
                           then executableMode
                           else nullMode

hasFileMode :: FileMode -> FileMode -> Bool
hasFileMode x y = x == intersectFileModes x y

isOwnerReadMode :: FileMode -> Bool
isOwnerReadMode = hasFileMode ownerReadMode

isOwnerWriteMode :: FileMode -> Bool
isOwnerWriteMode = hasFileMode ownerWriteMode

isOwnerExecuteMode :: FileMode -> Bool
isOwnerExecuteMode = hasFileMode ownerExecuteMode

isGroupReadMode :: FileMode -> Bool
isGroupReadMode = hasFileMode groupReadMode

isGroupWriteMode :: FileMode -> Bool
isGroupWriteMode = hasFileMode groupWriteMode

isGroupExecuteMode :: FileMode -> Bool
isGroupExecuteMode = hasFileMode groupExecuteMode

isOtherReadMode :: FileMode -> Bool
isOtherReadMode = hasFileMode otherReadMode

isOtherWriteMode :: FileMode -> Bool
isOtherWriteMode = hasFileMode otherWriteMode

isOtherExecuteMode :: FileMode -> Bool
isOtherExecuteMode = hasFileMode otherExecuteMode