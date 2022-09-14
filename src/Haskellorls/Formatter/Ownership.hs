module Haskellorls.Formatter.Ownership
  ( ownerName,
    normalColoredOwnerName,
    coloredOwnerName,
    groupName,
    normalColoredGroupName,
    coloredGroupName,
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
import qualified Haskellorls.System.Posix.PosixString as Posix
import Witch (From (from))
import Prelude hiding (lookup)

newtype UserIdSubstTable = UserIdSubstTable (IM.IntMap T.Text)
  deriving (Semigroup, Monoid)

instance From [Posix.UserEntry] UserIdSubstTable where
  from = UserIdSubstTable . IM.fromList . map (\entry -> (fromIntegral $ Posix.userID entry, T.pack $ Posix.userName entry))

instance Dictionary Posix.UserID T.Text UserIdSubstTable where
  lookup k (UserIdSubstTable m) = fromIntegral k `IM.lookup` m

getUserIdSubstTable :: IO UserIdSubstTable
getUserIdSubstTable = from <$> Posix.getAllUserEntries

newtype GroupIdSubstTable = GroupIdSubstTable (IM.IntMap T.Text)
  deriving (Semigroup, Monoid)

instance From [Posix.GroupEntry] GroupIdSubstTable where
  from = GroupIdSubstTable . IM.fromList . map (\entry -> (fromIntegral $ Posix.groupID entry, T.pack $ Posix.groupName entry))

instance Dictionary Posix.GroupID T.Text GroupIdSubstTable where
  lookup k (GroupIdSubstTable m) = fromIntegral k `IM.lookup` m

getGroupIdSubstTable :: IO GroupIdSubstTable
getGroupIdSubstTable = from <$> Posix.getAllGroupEntries

-- | A name of the UNIX user which identifies an user.
ownerName :: UserIdSubstTable -> Node.NodeInfo -> T.Text
ownerName table = maybe "?" (\uid -> fromMaybe (T.pack $ show uid) $ uid `lookup` table) . Node.userID

-- | A node owner name formatter for the @no@ parameter of @LS_COLORS@.
normalColoredOwnerName :: UserIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredOwnerName table lscolors node = [Attr.Other . WT.wrap lscolors Color.normal $ ownerName table node]

coloredOwnerName :: UserIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredOwnerName table lscolors user node =
  let getter = maybe (const Nothing) (\uid -> Color.lookup $ withUserContext uid user) $ Node.userID node
   in [Attr.Other . WT.wrap lscolors getter $ ownerName table node]

-- | A name of the UNIX group which an user belongs to.
groupName :: GroupIdSubstTable -> Node.NodeInfo -> T.Text
groupName table = maybe "?" (\gid -> fromMaybe (T.pack $ show gid) $ gid `lookup` table) . Node.groupID

-- | A node group name formatter for the @no@ parameter of @LS_COLORS@.
normalColoredGroupName :: GroupIdSubstTable -> Color.LsColors -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredGroupName table lscolors node = [Attr.Other . WT.wrap lscolors Color.normal $ groupName table node]

coloredGroupName :: GroupIdSubstTable -> Color.LsColors -> UserInfo -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredGroupName table lscolors user node =
  let getter = maybe (const Nothing) (\gid -> Color.lookup $ withGroupContext gid user) $ Node.groupID node
   in [Attr.Other . WT.wrap lscolors getter $ groupName table node]
