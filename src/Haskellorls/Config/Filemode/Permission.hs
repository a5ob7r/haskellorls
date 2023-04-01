module Haskellorls.Config.Filemode.Permission
  ( Permission,
    PermissionClass (..),
    PermissionContext (..),
    module Haskellorls.Class,
  )
where

import Haskellorls.Class (Dictionary (..))
import Haskellorls.System.Posix.PosixString qualified as Posix
import Witch (From (..), via)
import Prelude hiding (lookup)

-- | Permission class with @user@, @group@ and @other@ context.
type Permission = PermissionContext PermissionClass

instance From Permission Char where
  from = via @PermissionClass

-- | Permission context with @user@, @group@ and @other@.
data PermissionContext a = UserPerm a | GroupPerm a | OtherPerm a
  deriving (Eq, Show, Functor)

instance From (PermissionContext a) a where
  from (UserPerm a) = a
  from (GroupPerm a) = a
  from (OtherPerm a) = a

-- TODO: This usage is hacky.
instance Dictionary Permission PermissionClass Posix.FileMode where
  lookup p mode = case p of
    UserPerm READ | mode `Posix.hasFileMode` Posix.ownerReadMode -> Just READ
    UserPerm WRITE | mode `Posix.hasFileMode` Posix.ownerWriteMode -> Just WRITE
    UserPerm EXEC -> case (mode `Posix.hasFileMode` Posix.setUserIDMode, mode `Posix.hasFileMode` Posix.ownerExecuteMode) of
      (True, True) -> Just E_SETUID
      (True, _) -> Just SETUID
      (_, True) -> Just EXEC
      _ -> Nothing
    GroupPerm READ | mode `Posix.hasFileMode` Posix.groupReadMode -> Just READ
    GroupPerm WRITE | mode `Posix.hasFileMode` Posix.groupWriteMode -> Just WRITE
    GroupPerm EXEC -> case (mode `Posix.hasFileMode` Posix.setGroupIDMode, mode `Posix.hasFileMode` Posix.groupExecuteMode) of
      (True, True) -> Just E_SETGID
      (True, _) -> Just SETGID
      (_, True) -> Just EXEC
      _ -> Nothing
    OtherPerm READ | mode `Posix.hasFileMode` Posix.otherReadMode -> Just READ
    OtherPerm WRITE | mode `Posix.hasFileMode` Posix.otherWriteMode -> Just WRITE
    OtherPerm EXEC -> case (mode `Posix.hasFileMode` Posix.stickyMode, mode `Posix.hasFileMode` Posix.otherExecuteMode) of
      (True, True) -> Just E_STICKY
      (True, _) -> Just STICKY
      (_, True) -> Just EXEC
      _ -> Nothing
    _ -> Nothing

instance Dictionary Permission Permission Posix.FileMode where
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
  | MISSING
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
    MISSING -> '?'
