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

import qualified Data.IntMap.Strict as IM
import Data.Maybe
import qualified Data.Text as T
import Haskellorls.Class
import Haskellorls.Config.Ownership
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types
import qualified System.Posix.User as User
import Prelude hiding (lookup)

newtype UserIdSubstTable = UserIdSubstTable (IM.IntMap T.Text)
  deriving (Semigroup, Monoid)

instance From [User.UserEntry] UserIdSubstTable where
  from = UserIdSubstTable . IM.fromList . map (\entry -> (fromIntegral $ User.userID entry, T.pack $ User.userName entry))

instance Dictionary Types.UserID T.Text UserIdSubstTable where
  lookup k (UserIdSubstTable m) = fromIntegral k `IM.lookup` m

getUserIdSubstTable :: IO UserIdSubstTable
getUserIdSubstTable = from <$> User.getAllUserEntries

newtype GroupIdSubstTable = GroupIdSubstTable (IM.IntMap T.Text)
  deriving (Semigroup, Monoid)

instance From [User.GroupEntry] GroupIdSubstTable where
  from = GroupIdSubstTable . IM.fromList . map (\entry -> (fromIntegral $ User.groupID entry, T.pack $ User.groupName entry))

instance Dictionary Types.GroupID T.Text GroupIdSubstTable where
  lookup k (GroupIdSubstTable m) = fromIntegral k `IM.lookup` m

getGroupIdSubstTable :: IO GroupIdSubstTable
getGroupIdSubstTable = from <$> User.getAllGroupEntries

-- Owner name {{{
ownerName :: UserIdSubstTable -> Node.NodeInfo -> T.Text
ownerName table = maybe "?" (\uid -> fromMaybe (T.pack $ show uid) $ uid `lookup` table) . Node.userID

numericOwnerName :: Node.NodeInfo -> T.Text
numericOwnerName = T.pack . show . Node.userID

-- | A node owner name formatter for the @no@ parameter of @LS_COLORS@.
normalColoredOwnerName :: UserIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredOwnerName table lscolors node = [Attr.Other $ WT.wrap lscolors Color.normal name]
  where
    name = ownerName table node

coloredOwnerName :: UserIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredOwnerName table = coloredOwnerAs (ownerName table)

coloredNumericOwnerName :: Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredNumericOwnerName = coloredOwnerAs numericOwnerName

coloredOwnerAs :: (Node.NodeInfo -> T.Text) -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredOwnerAs f lscolors user node = [Attr.Other $ WT.wrap lscolors getter (f node)]
  where
    getter = case Node.userID node of
      Just uid
        | uid == userInfoUserID user -> Color.lookup $ Myself uid
        | otherwise -> Color.lookup $ NotMyself uid
      _ -> const Nothing

-- }}}

-- Group name {{{
groupName :: GroupIdSubstTable -> Node.NodeInfo -> T.Text
groupName table = maybe "?" (\gid -> fromMaybe (T.pack $ show gid) $ gid `lookup` table) . Node.groupID

numericGroupName :: Node.NodeInfo -> T.Text
numericGroupName = T.pack . show . Node.groupID

-- | A node group name formatter for the @no@ parameter of @LS_COLORS@.
normalColoredGroupName :: GroupIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredGroupName table lscolors node = [Attr.Other $ WT.wrap lscolors Color.normal name]
  where
    name = groupName table node

coloredGroupName :: GroupIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredGroupName table = coloredGroupAs (groupName table)

coloredNumericGroupName :: Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredNumericGroupName = coloredGroupAs numericGroupName

coloredGroupAs :: (Node.NodeInfo -> T.Text) -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredGroupAs f lscolors user node = [Attr.Other $ WT.wrap lscolors getter (f node)]
  where
    getter = case Node.groupID node of
      Just gid
        | gid `elem` userInfoGroupIDs user -> Color.lookup $ Belongs gid
        | otherwise -> Color.lookup $ NotBelongs gid
      Nothing -> const Nothing

-- }}}
