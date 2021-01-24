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

import qualified Data.Map.Strict as M (Map, fromList, (!))
import qualified Haskellorls.Color as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.UserInfo as UserInfo
import qualified Haskellorls.YetAnotherString as YAString (WrapedString (..))
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types
  ( GroupID,
    UserID,
  )
import qualified System.Posix.User as User
  ( GroupEntry (groupID, groupName),
    UserEntry (userID, userName),
    getAllGroupEntries,
    getAllUserEntries,
  )

type UserIdSubstTable = M.Map Types.UserID String

type GroupIdSubstTable = M.Map Types.GroupID String

-- Utilities {{{
lookupUserName :: Types.UserID -> UserIdSubstTable -> String
lookupUserName uid substTable = (M.!) substTable uid

lookupGroupName :: Types.GroupID -> GroupIdSubstTable -> String
lookupGroupName gid substTable = (M.!) substTable gid

getUserIdSubstTable :: IO UserIdSubstTable
getUserIdSubstTable = do
  entries <- User.getAllUserEntries
  return . M.fromList $ map (\entry -> (User.userID entry, User.userName entry)) entries

getGroupIdSubstTable :: IO GroupIdSubstTable
getGroupIdSubstTable = do
  entries <- User.getAllGroupEntries
  return . M.fromList $ map (\entry -> (User.groupID entry, User.groupName entry)) entries

-- }}}

-- Owner name {{{
ownerName :: UserIdSubstTable -> Node.NodeInfo -> String
ownerName table node = ownerID `lookupUserName` table
  where
    ownerID = numericOwnerName' node

numericOwnerName :: Node.NodeInfo -> String
numericOwnerName = show . numericOwnerName'

numericOwnerName' :: Node.NodeInfo -> Types.UserID
numericOwnerName' = Files.fileOwner . Node.nodeInfoStatus

coloredOwnerName :: UserIdSubstTable -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [YAString.WrapedString]
coloredOwnerName table = coloredOwnerAs (ownerName table)

coloredNumericOwnerName :: Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [YAString.WrapedString]
coloredNumericOwnerName = coloredOwnerAs numericOwnerName

coloredOwnerAs :: (Node.NodeInfo -> String) -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [YAString.WrapedString]
coloredOwnerAs f config user node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq,
        YAString.wrappedStringMain = f node,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    escSeq =
      if currentUserID == nodeOwnerID
        then Color.ownerYourselfEscapeSequence extConfig
        else Color.ownerNotYourselfEscapeSequence extConfig
    currentUserID = UserInfo.userInfoUserID user
    nodeOwnerID = Files.fileOwner $ Node.nodeInfoStatus node
    extConfig = Color.extensionColorConfig config

-- }}}

-- Group name {{{
groupName :: GroupIdSubstTable -> Node.NodeInfo -> String
groupName table node = groupID `lookupGroupName` table
  where
    groupID = numericGroupName' node

numericGroupName :: Node.NodeInfo -> String
numericGroupName = show . numericGroupName'

numericGroupName' :: Node.NodeInfo -> Types.GroupID
numericGroupName' = Files.fileGroup . Node.nodeInfoStatus

coloredGroupName :: GroupIdSubstTable -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [YAString.WrapedString]
coloredGroupName table = coloredGroupAs (groupName table)

coloredNumericGroupName :: Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [YAString.WrapedString]
coloredNumericGroupName = coloredGroupAs numericGroupName

coloredGroupAs :: (Node.NodeInfo -> String) -> Color.Config -> UserInfo.UserInfo -> Node.NodeInfo -> [YAString.WrapedString]
coloredGroupAs f config user node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq,
        YAString.wrappedStringMain = f node,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    escSeq =
      if nodeGroupID `elem` groupIDs
        then Color.groupYouBelongsToEscapeSequence extConfig
        else Color.groupYouNotBelongsToEscapeSequence extConfig
    groupIDs = UserInfo.userInfoGroupIDs user
    nodeGroupID = Files.fileGroup $ Node.nodeInfoStatus node
    extConfig = Color.extensionColorConfig config

-- }}}
