module Haskellorls.Config.Ownership
  ( UserInfo,
    userInfo,
    UserContext (..),
    withUserContext,
    GroupContext (..),
    withGroupContext,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Posix.Types (GroupID, UserID)
import System.Posix.User (getGroups, getRealUserID)

data UserInfo = UserInfo {userID :: UserID, groupIDs :: [GroupID]}

userInfo :: MonadIO m => m UserInfo
userInfo = do
  userID <- liftIO getRealUserID
  groupIDs <- liftIO getGroups
  return UserInfo {..}

-- | An UNIX user ID context information, whether or not the user ID identifies
-- the current user.
data UserContext a = Myself a | NotMyself a

-- | Modify an 'UserID' by 'UserContext'.
withUserContext :: UserID -> UserInfo -> UserContext UserID
withUserContext uid UserInfo {userID}
  | uid == userID = Myself uid
  | otherwise = NotMyself uid

-- | An UNIX group ID context information, whether or not the current user
-- belongs the group.
data GroupContext a = Belongs a | NotBelongs a

-- | Modify a 'GroupID' by 'GroupContext'.
withGroupContext :: GroupID -> UserInfo -> GroupContext GroupID
withGroupContext gid UserInfo {groupIDs}
  | gid `elem` groupIDs = Belongs gid
  | otherwise = NotBelongs gid
