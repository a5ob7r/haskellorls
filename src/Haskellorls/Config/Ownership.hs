module Haskellorls.Config.Ownership
  ( UserInfo (..),
    userInfo,
    UserID (..),
    GroupID (..),
  )
where

import qualified System.Posix.Types as Types
import qualified System.Posix.User as User

data UserInfo = UserInfo
  { userInfoUserID :: Types.UserID,
    userInfoGroupIDs :: [Types.GroupID]
  }

userInfo :: IO UserInfo
userInfo = do
  userInfoUserID <- User.getRealUserID
  userInfoGroupIDs <- User.getGroups
  return $ UserInfo {..}

data UserID = Myself Types.UserID | NotMyself Types.UserID

-- | A group ID with a context, whether or not the current user belongs the
-- group.
data GroupID = Belongs Types.GroupID | NotBelongs Types.GroupID
