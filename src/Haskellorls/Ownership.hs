-- Ownership provides utility functions which lookup owner id, owner name,
-- group id and group name. Also it provides coloring versions.
--
-- To color owner or group fields, need current user's id and group id, and to
-- compare the values to node's owner id and group id.
--
-- NOTE: owner also be called as user.

module Haskellorls.Ownership
  ( ownerName,
    numericOwnerName,
    coloredOwnerName,
    coloredNumericOwnerName,
    groupName,
    numericGroupName,
    coloredGroupName,
    coloredNumericGroupName,
    getGroupIdSubstTable,
    getUserIdSubstTable,
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.UserInfo as UserInfo
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types
import qualified System.Posix.User as User

type UserIdSubstTable = M.Map Types.UserID T.Text

type GroupIdSubstTable = M.Map Types.GroupID T.Text

-- Utilities {{{
lookupUserName :: Types.UserID -> UserIdSubstTable -> T.Text
lookupUserName uid = M.findWithDefault (T.pack $ show uid) uid

lookupGroupName :: Types.GroupID -> GroupIdSubstTable -> T.Text
lookupGroupName gid = M.findWithDefault (T.pack $ show gid) gid

getUserIdSubstTable :: IO UserIdSubstTable
getUserIdSubstTable = do
  entries <- User.getAllUserEntries
  return . M.fromList $ map (\entry -> (User.userID entry, T.pack $ User.userName entry)) entries

getGroupIdSubstTable :: IO GroupIdSubstTable
getGroupIdSubstTable = do
  entries <- User.getAllGroupEntries
  return . M.fromList $ map (\entry -> (User.groupID entry, T.pack $ User.groupName entry)) entries

-- }}}

-- Owner name {{{
ownerName :: UserIdSubstTable -> Node.NodeInfo -> T.Text
ownerName table node = ownerID `lookupUserName` table
  where
    ownerID = numericOwnerName' node

numericOwnerName :: Node.NodeInfo -> T.Text
numericOwnerName = T.pack . show . numericOwnerName'

numericOwnerName' :: Node.NodeInfo -> Types.UserID
numericOwnerName' = Files.fileOwner . Node.nodeInfoStatus

coloredOwnerName :: UserIdSubstTable -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredOwnerName table = coloredOwnerAs (ownerName table)

coloredNumericOwnerName :: Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredNumericOwnerName = coloredOwnerAs numericOwnerName

coloredOwnerAs :: (Node.NodeInfo -> T.Text) -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredOwnerAs f config user node = [Color.toWrappedText config getter (f node)]
  where
    getter
      | currentUserID == nodeOwnerID = Color.ownerYourselfEscapeSequence . Color.extensionColorConfig
      | otherwise = Color.ownerNotYourselfEscapeSequence . Color.extensionColorConfig
    currentUserID = UserInfo.userInfoUserID user
    nodeOwnerID = Files.fileOwner $ Node.nodeInfoStatus node

-- }}}

-- Group name {{{
groupName :: GroupIdSubstTable -> Node.NodeInfo -> T.Text
groupName table node = groupID `lookupGroupName` table
  where
    groupID = numericGroupName' node

numericGroupName :: Node.NodeInfo -> T.Text
numericGroupName = T.pack . show . numericGroupName'

numericGroupName' :: Node.NodeInfo -> Types.GroupID
numericGroupName' = Files.fileGroup . Node.nodeInfoStatus

coloredGroupName :: GroupIdSubstTable -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredGroupName table = coloredGroupAs (groupName table)

coloredNumericGroupName :: Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredNumericGroupName = coloredGroupAs numericGroupName

coloredGroupAs :: (Node.NodeInfo -> T.Text) -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredGroupAs f config user node = [Color.toWrappedText config getter (f node)]
  where
    getter
      | nodeGroupID `elem` groupIDs = Color.groupYouBelongsToEscapeSequence . Color.extensionColorConfig
      | otherwise = Color.groupYouNotBelongsToEscapeSequence . Color.extensionColorConfig
    groupIDs = UserInfo.userInfoGroupIDs user
    nodeGroupID = Files.fileGroup $ Node.nodeInfoStatus node

-- }}}
