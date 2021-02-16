module Haskellorls.Ownership.Type
  ( UserInfo (..),
    userInfo,
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
  uID <- User.getRealUserID
  gIDs <- User.getGroups
  return $
    UserInfo
      { userInfoUserID = uID,
        userInfoGroupIDs = gIDs
      }
