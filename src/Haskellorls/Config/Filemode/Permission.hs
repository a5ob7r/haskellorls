module Haskellorls.Config.Filemode.Permission
  ( Permission,
    PermissionClass (..),
    PermissionContext (..),
    module Haskellorls.Class,
  )
where

import Haskellorls.Class
import qualified System.Posix.Files.ByteString as Files
import qualified System.Posix.Types as Types
import Prelude hiding (lookup)

-- | Permission class with @user@, @group@ and @other@ context.
type Permission = PermissionContext PermissionClass

instance From Permission Char where
  from = from @PermissionClass . from

-- | Permission context with @user@, @group@ and @other@.
data PermissionContext a = UserPerm a | GroupPerm a | OtherPerm a
  deriving (Eq, Show, Functor)

instance From (PermissionContext a) a where
  from (UserPerm a) = a
  from (GroupPerm a) = a
  from (OtherPerm a) = a

-- TODO: This usage is hacky.
instance Dictionary Permission PermissionClass Types.FileMode where
  lookup p mode = case p of
    UserPerm READ | mode `hasFileMode` Files.ownerReadMode -> Just READ
    UserPerm WRITE | mode `hasFileMode` Files.ownerWriteMode -> Just WRITE
    UserPerm EXEC -> case (mode `hasFileMode` Files.setUserIDMode, mode `hasFileMode` Files.ownerExecuteMode) of
      (True, True) -> Just E_SETUID
      (True, _) -> Just SETUID
      (_, True) -> Just EXEC
      _ -> Nothing
    GroupPerm READ | mode `hasFileMode` Files.groupReadMode -> Just READ
    GroupPerm WRITE | mode `hasFileMode` Files.groupWriteMode -> Just WRITE
    GroupPerm EXEC -> case (mode `hasFileMode` Files.setGroupIDMode, mode `hasFileMode` Files.groupExecuteMode) of
      (True, True) -> Just E_SETGID
      (True, _) -> Just SETGID
      (_, True) -> Just EXEC
      _ -> Nothing
    OtherPerm READ | mode `hasFileMode` Files.otherReadMode -> Just READ
    OtherPerm WRITE | mode `hasFileMode` Files.otherWriteMode -> Just WRITE
    OtherPerm EXEC -> case (mode `hasFileMode` stickyMode, mode `hasFileMode` Files.otherExecuteMode) of
      (True, True) -> Just E_STICKY
      (True, _) -> Just STICKY
      (_, True) -> Just EXEC
      _ -> Nothing
    _ -> Nothing
    where
      hasFileMode x y = y == Files.intersectFileModes x y
      stickyMode = 548

instance Dictionary Permission Permission Types.FileMode where
  lookup p mode = case p of
    UserPerm _ -> UserPerm <$> p `lookup` mode
    GroupPerm _ -> GroupPerm <$> p `lookup` mode
    OtherPerm _ -> OtherPerm <$> p `lookup` mode

data PermissionClass
  = READ
  | WRITE
  | EXEC
  | SETUID
  | SETGID
  | STICKY
  | E_SETUID
  | E_SETGID
  | E_STICKY
  | NOTHING
  deriving (Eq, Show)

instance From PermissionClass Char where
  from = \case
    READ -> 'r'
    WRITE -> 'w'
    EXEC -> 'x'
    SETUID -> 'S'
    SETGID -> 'S'
    STICKY -> 'T'
    E_SETUID -> 's'
    E_SETGID -> 's'
    E_STICKY -> 't'
    NOTHING -> '-'
