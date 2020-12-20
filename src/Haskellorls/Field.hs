module Haskellorls.Field
  ( FilemodeField(..)
  , filemodeField
  , fileTypeLetter
  , filemodeBitPatternTypeLetter
  , showFilemodeField
  ) where

import System.Posix.Files
    ( fileMode
    , groupExecuteMode
    , groupReadMode
    , groupWriteMode
    , intersectFileModes
    , isBlockDevice
    , isCharacterDevice
    , isDirectory
    , isNamedPipe
    , isRegularFile
    , isSocket
    , isSymbolicLink
    , otherExecuteMode
    , otherReadMode
    , otherWriteMode
    , ownerExecuteMode
    , ownerReadMode
    , ownerWriteMode
    , setGroupIDMode
    , setUserIDMode
    , FileStatus
    )
import System.Posix.Types ( FileMode )

data FilemodeBitPatternType
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

{-
   TODO: some kinds of devices are not implemented.
-}
data FileType
  = REGULAR
  | BLOCK
  | CHAR
  | DIR
  | SYMLINK
  | FIFO
  | SOCK
  | OTHER

data FilemodeField = FilemodeField
  { getFiletype :: FileType
  , getUserRead :: FilemodeBitPatternType
  , getUserWrite :: FilemodeBitPatternType
  , getUserExec :: FilemodeBitPatternType
  , getGroupRead :: FilemodeBitPatternType
  , getGroupWrite :: FilemodeBitPatternType
  , getGroupExec :: FilemodeBitPatternType
  , getOtherRead :: FilemodeBitPatternType
  , getOtherWrite :: FilemodeBitPatternType
  , getOtherExec :: FilemodeBitPatternType
  }

filemodeField :: FileStatus -> FilemodeField
filemodeField status = FilemodeField
  { getFiletype = fileType status
  , getUserRead = userReadModeType mode
  , getUserWrite = userWriteModeType mode
  , getUserExec = userExecModeType mode
  , getGroupRead = groupReadModeType mode
  , getGroupWrite = groupWriteModeType mode
  , getGroupExec = groupExecModeType mode
  , getOtherRead = otherReadModeType mode
  , getOtherWrite = otherWriteModeType mode
  , getOtherExec = otherExecModeType mode
  }
    where
      mode = fileMode status

showFilemodeField :: FilemodeField -> String
showFilemodeField field = fType:permFields
  where
    fType = fileTypeLetter $ getFiletype field
    permFields = map (\f -> filemodeBitPatternTypeLetter $ f field) permLetterFuncs

permLetterFuncs :: [FilemodeField -> FilemodeBitPatternType]
permLetterFuncs =
  [ getUserRead
  , getUserWrite
  , getUserExec
  , getGroupRead
  , getGroupWrite
  , getGroupExec
  , getOtherRead
  , getOtherWrite
  , getOtherExec
  ]

-- File type field charactor {{{
regularFileLetter :: Char
regularFileLetter = '-'

blockLetter :: Char
blockLetter = 'b'

charLetter :: Char
charLetter = 'c'

directoryLetter :: Char
directoryLetter = 'd'

symlinkLetter :: Char
symlinkLetter = 'l'

fifoLetter :: Char
fifoLetter = 'p'

socketLetter :: Char
socketLetter = 's'

otherLetter :: Char
otherLetter = '?'
-- }}}

-- Utilities for file type {{{
fileType :: FileStatus -> FileType
fileType status
  | isRegularFile status = REGULAR
  | isDirectory status = DIR
  | isBlockDevice status = BLOCK
  | isCharacterDevice status = CHAR
  | isSymbolicLink status = SYMLINK
  | isNamedPipe status = FIFO
  | isSocket status = SOCK
  | otherwise = OTHER

fileTypeLetter :: FileType -> Char
fileTypeLetter fType = case fType of
                         REGULAR -> regularFileLetter
                         BLOCK -> blockLetter
                         CHAR -> charLetter
                         DIR -> directoryLetter
                         SYMLINK -> symlinkLetter
                         FIFO -> fifoLetter
                         SOCK -> socketLetter
                         OTHER -> otherLetter
-- }}}

-- File mode field charactor {{{
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
-- }}}

-- Utilities to get file mode bit type {{{
userReadModeType :: FileMode -> FilemodeBitPatternType
userReadModeType mode
  | hasUserReadMode mode = READ
  | otherwise = NOTHING

userWriteModeType :: FileMode -> FilemodeBitPatternType
userWriteModeType mode
  | hasUserWriteMode mode = WRITE
  | otherwise = NOTHING

userExecModeType :: FileMode -> FilemodeBitPatternType
userExecModeType mode
  | isSetuid && isExec = E_SETUID
  | isSetuid = SETUID
  | isExec = EXEC
  | otherwise = NOTHING
  where
    isExec = hasUserExecMode mode
    isSetuid = hasSetuidMode mode

groupReadModeType :: FileMode -> FilemodeBitPatternType
groupReadModeType mode
  | hasGroupReadMode mode = READ
  | otherwise = NOTHING

groupWriteModeType :: FileMode -> FilemodeBitPatternType
groupWriteModeType mode
  | hasGroupWriteMode mode = READ
  | otherwise = NOTHING

groupExecModeType :: FileMode -> FilemodeBitPatternType
groupExecModeType mode
  | isSetgid && isExec = E_SETGID
  | isSetgid = SETGID
  | isExec = EXEC
  | otherwise = NOTHING
  where
    isExec = hasGroupExecMode mode
    isSetgid = hasSetgidMode mode

otherReadModeType :: FileMode -> FilemodeBitPatternType
otherReadModeType mode
  | hasOtherReadMode mode = READ
  | otherwise = NOTHING

otherWriteModeType :: FileMode -> FilemodeBitPatternType
otherWriteModeType mode
  | hasOtherWriteMode mode = WRITE
  | otherwise = NOTHING

otherExecModeType :: FileMode -> FilemodeBitPatternType
otherExecModeType mode
  | isSticky && isExec = E_STICKY
  | isSticky = STICKY
  | isExec = EXEC
  | otherwise = NOTHING
  where
    isExec = hasOtherExecMode mode
    isSticky = hasStickyMode mode

filemodeBitPatternTypeLetter :: FilemodeBitPatternType -> Char
filemodeBitPatternTypeLetter ptnType = case ptnType of
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
-- }}}

-- Utilities to get whether or not has a file mode {{{
hasUserReadMode :: FileMode -> Bool
hasUserReadMode mode = mode `hasFileMode` ownerReadMode

hasUserWriteMode :: FileMode -> Bool
hasUserWriteMode mode = mode `hasFileMode` ownerWriteMode

hasUserExecMode :: FileMode -> Bool
hasUserExecMode mode = mode `hasFileMode` ownerExecuteMode

hasSetuidMode :: FileMode -> Bool
hasSetuidMode mode = mode `hasFileMode` setUserIDMode

hasGroupReadMode :: FileMode -> Bool
hasGroupReadMode mode = mode `hasFileMode` groupReadMode

hasGroupWriteMode :: FileMode -> Bool
hasGroupWriteMode mode = mode `hasFileMode` groupWriteMode

hasGroupExecMode :: FileMode -> Bool
hasGroupExecMode mode = mode `hasFileMode` groupExecuteMode

hasSetgidMode :: FileMode -> Bool
hasSetgidMode mode = mode `hasFileMode` setGroupIDMode

hasOtherReadMode :: FileMode -> Bool
hasOtherReadMode mode = mode `hasFileMode` otherReadMode

hasOtherWriteMode :: FileMode -> Bool
hasOtherWriteMode mode = mode `hasFileMode` otherWriteMode

hasOtherExecMode :: FileMode -> Bool
hasOtherExecMode mode = mode `hasFileMode` otherExecuteMode

hasStickyMode :: FileMode -> Bool
hasStickyMode mode = mode `hasFileMode` stickyMode

hasFileMode :: FileMode -> FileMode -> Bool
hasFileMode x y = y == intersectFileModes x y

stickyMode :: FileMode
stickyMode = 548
-- }}}
