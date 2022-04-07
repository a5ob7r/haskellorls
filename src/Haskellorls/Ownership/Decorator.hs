-- Ownership provides utility functions which lookup owner id, owner name,
-- group id and group name. Also it provides coloring versions.
--
-- To color owner or group fields, need current user's id and group id, and to
-- compare the values to node's owner id and group id.
--
-- NOTE: owner also be called as user.

module Haskellorls.Ownership.Decorator
  ( ownerName,
    numericOwnerName,
    normalColoredOwnerName,
    coloredOwnerName,
    coloredNumericOwnerName,
    groupName,
    numericGroupName,
    normalColoredGroupName,
    coloredGroupName,
    coloredNumericGroupName,
    getGroupIdSubstTable,
    getUserIdSubstTable,
    module Data.Default,
    module Haskellorls.Ownership.Type,
  )
where

import Data.Default
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Haskellorls.Class
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import Haskellorls.Ownership.Type
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Types as Types
import qualified System.Posix.User as User
import Prelude hiding (lookup)

newtype UserIdSubstTable = UserIdSubstTable {unUserIdSubstTable :: M.Map Types.UserID T.Text}
  deriving (Default)

newtype GroupIdSubstTable = GroupIdSubstTable {unGroupIdSubstTable :: M.Map Types.GroupID T.Text}
  deriving (Default)

-- Utilities {{{
instance From Types.UserID T.Text where
  from = T.pack . show

instance Serialize Types.UserID

instance From Types.GroupID T.Text where
  from = T.pack . show

instance Serialize Types.GroupID

instance Dictionary Types.UserID T.Text UserIdSubstTable where
  lookup k (UserIdSubstTable m) = k `M.lookup` m

instance Dictionary Types.GroupID T.Text GroupIdSubstTable where
  lookup k (GroupIdSubstTable m) = k `M.lookup` m

getUserIdSubstTable :: IO UserIdSubstTable
getUserIdSubstTable = do
  entries <- User.getAllUserEntries
  pure . UserIdSubstTable . M.fromList $ map (\entry -> (User.userID entry, T.pack $ User.userName entry)) entries

getGroupIdSubstTable :: IO GroupIdSubstTable
getGroupIdSubstTable = do
  entries <- User.getAllGroupEntries
  pure . GroupIdSubstTable . M.fromList $ map (\entry -> (User.groupID entry, T.pack $ User.groupName entry)) entries

instance From Node.NodeInfo Types.UserID where
  from = Node.pfsUserID . Node.getNodeStatus

-- }}}

-- Owner name {{{
ownerName :: UserIdSubstTable -> Node.NodeInfo -> T.Text
ownerName table node = fromMaybe (serialize ownerID) $ ownerID `lookup` table
  where
    ownerID :: Types.UserID
    ownerID = from node

numericOwnerName :: Node.NodeInfo -> T.Text
numericOwnerName = T.pack . show . (from :: Node.NodeInfo -> Types.UserID)

-- | A node owner name decorator for the @no@ parameter of @LS_COLORS@.
normalColoredOwnerName :: UserIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
normalColoredOwnerName table lscolors node = [Color.toWrappedText lscolors Color.normal name]
  where
    name = ownerName table node

coloredOwnerName :: UserIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredOwnerName table = coloredOwnerAs (ownerName table)

coloredNumericOwnerName :: Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredNumericOwnerName = coloredOwnerAs numericOwnerName

coloredOwnerAs :: (Node.NodeInfo -> T.Text) -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredOwnerAs f lscolors user node = [Color.toWrappedText lscolors getter (f node)]
  where
    getter
      | currentUserID == nodeOwnerID = Color.lookup $ Myself nodeOwnerID
      | otherwise = Color.lookup $ NotMyself nodeOwnerID
    currentUserID = userInfoUserID user
    nodeOwnerID = Node.pfsUserID $ Node.getNodeStatus node

-- }}}

-- Group name {{{
groupName :: GroupIdSubstTable -> Node.NodeInfo -> T.Text
groupName table node = fromMaybe (serialize groupID) $ groupID `lookup` table
  where
    groupID = numericGroupName' node

numericGroupName :: Node.NodeInfo -> T.Text
numericGroupName = T.pack . show . numericGroupName'

numericGroupName' :: Node.NodeInfo -> Types.GroupID
numericGroupName' = Node.pfsGroupID . Node.getNodeStatus

-- | A node group name decorator for the @no@ parameter of @LS_COLORS@.
normalColoredGroupName :: GroupIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
normalColoredGroupName table lscolors node = [Color.toWrappedText lscolors Color.normal name]
  where
    name = groupName table node

coloredGroupName :: GroupIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredGroupName table = coloredGroupAs (groupName table)

coloredNumericGroupName :: Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredNumericGroupName = coloredGroupAs numericGroupName

coloredGroupAs :: (Node.NodeInfo -> T.Text) -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredGroupAs f lscolors user node = [Color.toWrappedText lscolors getter (f node)]
  where
    getter
      | nodeGroupID `elem` groupIDs = Color.lookup $ Belongs nodeGroupID
      | otherwise = Color.lookup $ NotBelongs nodeGroupID
    groupIDs = userInfoGroupIDs user
    nodeGroupID = Node.pfsGroupID $ Node.getNodeStatus node

-- }}}
