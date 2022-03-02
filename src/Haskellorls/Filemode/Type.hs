module Haskellorls.Filemode.Type
  ( Filemode (..),
    module Haskellorls.Filemode.Entry,
    module Haskellorls.Filemode.Permission,
  )
where

import qualified Data.Maybe as Maybe
import Haskellorls.Filemode.Entry
import Haskellorls.Filemode.Permission
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

instance From ProxyFileStatus Filemode where
  from status =
    Filemode
      { getFiletype = from status,
        getUserRead = Maybe.fromMaybe NOTHING $ UserPerm READ `lookup` mode,
        getUserWrite = Maybe.fromMaybe NOTHING $ UserPerm WRITE `lookup` mode,
        getUserExec = Maybe.fromMaybe NOTHING $ UserPerm EXEC `lookup` mode,
        getGroupRead = Maybe.fromMaybe NOTHING $ GroupPerm READ `lookup` mode,
        getGroupWrite = Maybe.fromMaybe NOTHING $ GroupPerm WRITE `lookup` mode,
        getGroupExec = Maybe.fromMaybe NOTHING $ GroupPerm EXEC `lookup` mode,
        getOtherRead = Maybe.fromMaybe NOTHING $ OtherPerm READ `lookup` mode,
        getOtherWrite = Maybe.fromMaybe NOTHING $ OtherPerm WRITE `lookup` mode,
        getOtherExec = Maybe.fromMaybe NOTHING $ OtherPerm EXEC `lookup` mode
      }
    where
      mode = pfsFileMode status
