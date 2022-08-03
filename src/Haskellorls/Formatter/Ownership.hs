-- Ownership provides utility functions which lookup owner id, owner name,
-- group id and group name. Also it provides coloring versions.
--
-- To color owner or group fields, need current user's id and group id, and to
-- compare the values to node's owner id and group id.
--
-- NOTE: owner also be called as user.

module Haskellorls.Formatter.Ownership
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
    module Haskellorls.Config.Ownership,
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Haskellorls.Class
import Haskellorls.Config.Ownership
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types
import qualified System.Posix.User as User
import Prelude hiding (lookup)

newtype UserIdSubstTable = UserIdSubstTable (M.Map Types.UserID T.Text)
  deriving (Semigroup, Monoid)

instance Dictionary Types.UserID T.Text UserIdSubstTable where
  lookup k (UserIdSubstTable m) = k `M.lookup` m

getUserIdSubstTable :: IO UserIdSubstTable
getUserIdSubstTable = do
  entries <- User.getAllUserEntries
  pure . UserIdSubstTable . M.fromList $ map (\entry -> (User.userID entry, T.pack $ User.userName entry)) entries

newtype GroupIdSubstTable = GroupIdSubstTable (M.Map Types.GroupID T.Text)
  deriving (Semigroup, Monoid)

instance Dictionary Types.GroupID T.Text GroupIdSubstTable where
  lookup k (GroupIdSubstTable m) = k `M.lookup` m

getGroupIdSubstTable :: IO GroupIdSubstTable
getGroupIdSubstTable = do
  entries <- User.getAllGroupEntries
  pure . GroupIdSubstTable . M.fromList $ map (\entry -> (User.groupID entry, T.pack $ User.groupName entry)) entries

-- Owner name {{{
ownerName :: UserIdSubstTable -> Node.NodeInfo -> T.Text
ownerName table node = fromMaybe (T.pack $ show ownerID) $ ownerID `lookup` table
  where
    ownerID = Node.userID node

numericOwnerName :: Node.NodeInfo -> T.Text
numericOwnerName = T.pack . show . Node.userID

-- | A node owner name formatter for the @no@ parameter of @LS_COLORS@.
normalColoredOwnerName :: UserIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
normalColoredOwnerName table lscolors node = [WT.wrap lscolors Color.normal name]
  where
    name = ownerName table node

coloredOwnerName :: UserIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredOwnerName table = coloredOwnerAs (ownerName table)

coloredNumericOwnerName :: Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredNumericOwnerName = coloredOwnerAs numericOwnerName

coloredOwnerAs :: (Node.NodeInfo -> T.Text) -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredOwnerAs f lscolors user node = [WT.wrap lscolors getter (f node)]
  where
    getter
      | currentUserID == nodeOwnerID = Color.lookup $ Myself nodeOwnerID
      | otherwise = Color.lookup $ NotMyself nodeOwnerID
    currentUserID = userInfoUserID user
    nodeOwnerID = Node.userID node

-- }}}

-- Group name {{{
groupName :: GroupIdSubstTable -> Node.NodeInfo -> T.Text
groupName table node = fromMaybe (T.pack $ show groupID) $ groupID `lookup` table
  where
    groupID = Node.groupID node

numericGroupName :: Node.NodeInfo -> T.Text
numericGroupName = T.pack . show . Node.groupID

-- | A node group name formatter for the @no@ parameter of @LS_COLORS@.
normalColoredGroupName :: GroupIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [WT.WrappedText]
normalColoredGroupName table lscolors node = [WT.wrap lscolors Color.normal name]
  where
    name = groupName table node

coloredGroupName :: GroupIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredGroupName table = coloredGroupAs (groupName table)

coloredNumericGroupName :: Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredNumericGroupName = coloredGroupAs numericGroupName

coloredGroupAs :: (Node.NodeInfo -> T.Text) -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [WT.WrappedText]
coloredGroupAs f lscolors user node = [WT.wrap lscolors getter (f node)]
  where
    getter
      | nodeGroupID `elem` groupIDs = Color.lookup $ Belongs nodeGroupID
      | otherwise = Color.lookup $ NotBelongs nodeGroupID
    groupIDs = userInfoGroupIDs user
    nodeGroupID = Node.groupID node

-- }}}
