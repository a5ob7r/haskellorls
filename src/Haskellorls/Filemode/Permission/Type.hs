module Haskellorls.Filemode.Permission.Type
  ( Permission,
    PermissionClass (..),
    PermissionContext (..),
    module Haskellorls.Class,
  )
where

import Haskellorls.Class
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types
import Prelude hiding (lookup)

-- | Permission class with user/group/other context.
type Permission = PermissionContext PermissionClass

instance From Permission Char where
  from = from @PermissionClass . from

-- | Permission context such as user/group/other.
data PermissionContext a = UserPerm a | GroupPerm a | OtherPerm a
  deriving (Eq, Show, Functor)

instance From (PermissionContext a) a where
  from (UserPerm a) = a
  from (GroupPerm a) = a
  from (OtherPerm a) = a

-- TODO: This usage is hacky.
instance Dictionary Permission PermissionClass Types.FileMode where
  lookup p mode = case p of
    UserPerm READ | hasUserReadMode mode -> Just READ
    UserPerm WRITE | hasUserWriteMode mode -> Just WRITE
    UserPerm EXEC -> case (hasSetuidMode mode, hasUserExecMode mode) of
      (True, True) -> Just E_SETUID
      (True, _) -> Just SETUID
      (_, True) -> Just EXEC
      _ -> Nothing
    GroupPerm READ | hasGroupReadMode mode -> Just READ
    GroupPerm WRITE | hasGroupWriteMode mode -> Just WRITE
    GroupPerm EXEC -> case (hasSetgidMode mode, hasGroupExecMode mode) of
      (True, True) -> Just E_SETGID
      (True, _) -> Just SETGID
      (_, True) -> Just EXEC
      _ -> Nothing
    OtherPerm READ | hasOtherReadMode mode -> Just READ
    OtherPerm WRITE | hasOtherWriteMode mode -> Just WRITE
    OtherPerm EXEC -> case (hasStickyMode mode, hasOtherExecMode mode) of
      (True, True) -> Just E_STICKY
      (True, _) -> Just STICKY
      (_, True) -> Just EXEC
      _ -> Nothing
    _ -> Nothing

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
    READ -> readFieldLetter
    WRITE -> writeFieldLetter
    EXEC -> execFieldLetter
    SETUID -> setxidNoExecFieldLetter
    SETGID -> setxidNoExecFieldLetter
    STICKY -> stickyNoExecFieldLetter
    E_SETUID -> setxidExecFieldLetter
    E_SETGID -> setxidExecFieldLetter
    E_STICKY -> stickyExecFieldLetter
    NOTHING -> noFieldLetter

noFieldLetter :: Char
noFieldLetter = '-'

readFieldLetter :: Char
readFieldLetter = 'r'

writeFieldLetter :: Char
writeFieldLetter = 'w'

execFieldLetter :: Char
execFieldLetter = 'x'

setxidExecFieldLetter :: Char
setxidExecFieldLetter = 's'

setxidNoExecFieldLetter :: Char
setxidNoExecFieldLetter = 'S'

stickyExecFieldLetter :: Char
stickyExecFieldLetter = 't'

stickyNoExecFieldLetter :: Char
stickyNoExecFieldLetter = 'T'

hasUserReadMode :: Types.FileMode -> Bool
hasUserReadMode mode = mode `hasFileMode` Files.ownerReadMode

hasUserWriteMode :: Types.FileMode -> Bool
hasUserWriteMode mode = mode `hasFileMode` Files.ownerWriteMode

hasUserExecMode :: Types.FileMode -> Bool
hasUserExecMode mode = mode `hasFileMode` Files.ownerExecuteMode

hasSetuidMode :: Types.FileMode -> Bool
hasSetuidMode mode = mode `hasFileMode` Files.setUserIDMode

hasGroupReadMode :: Types.FileMode -> Bool
hasGroupReadMode mode = mode `hasFileMode` Files.groupReadMode

hasGroupWriteMode :: Types.FileMode -> Bool
hasGroupWriteMode mode = mode `hasFileMode` Files.groupWriteMode

hasGroupExecMode :: Types.FileMode -> Bool
hasGroupExecMode mode = mode `hasFileMode` Files.groupExecuteMode

hasSetgidMode :: Types.FileMode -> Bool
hasSetgidMode mode = mode `hasFileMode` Files.setGroupIDMode

hasOtherReadMode :: Types.FileMode -> Bool
hasOtherReadMode mode = mode `hasFileMode` Files.otherReadMode

hasOtherWriteMode :: Types.FileMode -> Bool
hasOtherWriteMode mode = mode `hasFileMode` Files.otherWriteMode

hasOtherExecMode :: Types.FileMode -> Bool
hasOtherExecMode mode = mode `hasFileMode` Files.otherExecuteMode

hasStickyMode :: Types.FileMode -> Bool
hasStickyMode mode = mode `hasFileMode` stickyMode

hasFileMode :: Types.FileMode -> Types.FileMode -> Bool
hasFileMode x y = y == Files.intersectFileModes x y

stickyMode :: Types.FileMode
stickyMode = 548
