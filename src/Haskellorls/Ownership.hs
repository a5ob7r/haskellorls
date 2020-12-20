module Haskellorls.Ownership
  ( lookupGroupName
  , lookupUserName
  , getGroupIdSubstTable
  , getUserIdSubstTable
  ) where

import qualified System.Posix.Types as Types
    ( UserID
    , GroupID
    )
import qualified System.Posix.User as User (UserEntry(userID), GroupEntry(groupID))
import System.Posix.User
    ( getAllGroupEntries
    , getAllUserEntries
    , GroupEntry(groupName)
    , UserEntry(userName)
    )
import qualified Data.Map.Strict as M (Map, fromList, (!))

type UserIdSubstTable = M.Map Types.UserID String
type GroupIdSubstTable = M.Map Types.GroupID String

lookupUserName :: Types.UserID -> UserIdSubstTable -> String
lookupUserName uid substTable = (M.!) substTable uid

lookupGroupName :: Types.GroupID -> GroupIdSubstTable -> String
lookupGroupName gid substTable = (M.!) substTable gid

getUserIdSubstTable :: IO UserIdSubstTable
getUserIdSubstTable = do
  entries <- getAllUserEntries
  return . M.fromList $ map (\entry -> (User.userID entry, userName entry)) entries

getGroupIdSubstTable :: IO GroupIdSubstTable
getGroupIdSubstTable = do
  entries <- getAllGroupEntries
  return . M.fromList $ map (\entry -> (User.groupID entry, groupName entry)) entries
