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
  from node = case nodeType node of
    Nothing -> Filemode SYMLINK MISSING MISSING MISSING MISSING MISSING MISSING MISSING MISSING MISSING
    _ ->
      let mode = fileMode node
          getFiletype = from node
          getUserRead = fromMaybe NOTHING $ mode >>= lookup (UserPerm READ)
          getUserWrite = fromMaybe NOTHING $ mode >>= lookup (UserPerm WRITE)
          getUserExec = fromMaybe NOTHING $ mode >>= lookup (UserPerm EXEC)
          getGroupRead = fromMaybe NOTHING $ mode >>= lookup (GroupPerm READ)
          getGroupWrite = fromMaybe NOTHING $ mode >>= lookup (GroupPerm WRITE)
          getGroupExec = fromMaybe NOTHING $ mode >>= lookup (GroupPerm EXEC)
          getOtherRead = fromMaybe NOTHING $ mode >>= lookup (OtherPerm READ)
          getOtherWrite = fromMaybe NOTHING $ mode >>= lookup (OtherPerm WRITE)
          getOtherExec = fromMaybe NOTHING $ mode >>= lookup (OtherPerm EXEC)
       in Filemode {..}
