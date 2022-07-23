module Haskellorls.Config.Filemode
  ( Filemode (..),
    module Haskellorls.Config.Filemode.Entry,
    module Haskellorls.Config.Filemode.Permission,
  )
where

import Data.Maybe
import Haskellorls.Config.Filemode.Entry
import Haskellorls.Config.Filemode.Permission
import Haskellorls.NodeInfo
import Prelude hiding (lookup)

data Filemode = Filemode
  { getFiletype :: EntryType,
    getUserRead :: PermissionClass,
    getUserWrite :: PermissionClass,
    getUserExec :: PermissionClass,
    getGroupRead :: PermissionClass,
    getGroupWrite :: PermissionClass,
    getGroupExec :: PermissionClass,
    getOtherRead :: PermissionClass,
    getOtherWrite :: PermissionClass,
    getOtherExec :: PermissionClass
  }
  deriving (Show)

instance From NodeInfo Filemode where
  from node =
    Filemode
      { getFiletype = from node,
        getUserRead = fromMaybe NOTHING $ UserPerm READ `lookup` mode,
        getUserWrite = fromMaybe NOTHING $ UserPerm WRITE `lookup` mode,
        getUserExec = fromMaybe NOTHING $ UserPerm EXEC `lookup` mode,
        getGroupRead = fromMaybe NOTHING $ GroupPerm READ `lookup` mode,
        getGroupWrite = fromMaybe NOTHING $ GroupPerm WRITE `lookup` mode,
        getGroupExec = fromMaybe NOTHING $ GroupPerm EXEC `lookup` mode,
        getOtherRead = fromMaybe NOTHING $ OtherPerm READ `lookup` mode,
        getOtherWrite = fromMaybe NOTHING $ OtherPerm WRITE `lookup` mode,
        getOtherExec = fromMaybe NOTHING $ OtherPerm EXEC `lookup` mode
      }
    where
      mode = fileMode node
