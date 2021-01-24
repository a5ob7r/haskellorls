module Haskellorls.UserInfo
  ( UserInfo (..),
    userInfo,
  )
where

import qualified System.Posix.Types as Types (GroupID, UserID)
import qualified System.Posix.User as User (getGroups, getRealUserID)

data UserInfo = UserInfo
  { userInfoUserID :: Types.UserID,
    userInfoGroupIDs :: [Types.GroupID]
  }

userInfo :: IO UserInfo
userInfo = do
  uID <- User.getRealUserID
  gIDs <- User.getGroups
  return $
    UserInfo
      { userInfoUserID = uID,
        userInfoGroupIDs = gIDs
      }
