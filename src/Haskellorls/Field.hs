module Haskellorls.Field
  ( FilemodeField(..)
  , filemodeField
  , fileTypeLetter
  , filemodeBitPatternTypeLetter
  , showFilemodeField
  , showFilemodeFieldWithColor
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

import qualified Haskellorls.Color as Color
  ( Config(..)
  , ExtensionConfig (..)
  , applyEscapeSequence
  )
import qualified Haskellorls.YetAnotherString as YAString (WrapedString (..))

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

showFilemodeField :: FilemodeField -> [YAString.WrapedString]
showFilemodeField field =
  [YAString.WrapedString { YAString.wrappedStringPrefix = ""
                         , YAString.wrappedStringMain = fType:permFields
                         , YAString.wrappedStringSuffix = ""
                         }
  ]
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

showFilemodeFieldWithColor :: Color.Config -> FilemodeField -> [YAString.WrapedString]
showFilemodeFieldWithColor config field =
  [ fType
  , userReadLetter
  , userWriteLetter
  , userExecLetter
  , groupReadLetter
  , groupWriteLetter
  , groupExecLetter
  , otherReadLetter
  , otherWriteLetter
  , otherExecLetter
  ]
  where
    fType = filetypeLetterWithColor config $ getFiletype field
    userReadLetter = userReadLetterWithColor config $ getUserRead field
    userWriteLetter = userWriteLetterWithColor config $ getUserWrite field
    userExecLetter = userExecLetterWithColor config $ getUserExec field
    groupReadLetter = groupReadLetterWithColor config $ getGroupRead field
    groupWriteLetter = groupWriteLetterWithColor config $ getGroupWrite field
    groupExecLetter = groupExecLetterWithColor config $ getGroupExec field
    otherReadLetter = otherReadLetterWithColor config $ getOtherRead field
    otherWriteLetter = otherWriteLetterWithColor config $ getOtherWrite field
    otherExecLetter = otherExecLetterWithColor config $ getOtherExec field

filetypeLetterWithColor :: Color.Config -> FileType -> YAString.WrapedString
filetypeLetterWithColor config filetype = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [fType]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    fType = fileTypeLetter filetype
    escSeq = fileTypeEscapeSequence config filetype

userReadLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
userReadLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
               READ -> Color.userReadPermBitEscapeSequence $ Color.extensionColorConfig config
               _ -> ""

userWriteLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
userWriteLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
               WRITE -> Color.userWritePermBitEscapeSequence $ Color.extensionColorConfig config
               _ -> ""

userExecLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
userExecLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
      E_SETUID -> Color.setuidEscapeSequence config
      SETUID -> Color.setuidEscapeSequence config
      EXEC -> Color.userExecPermBitFileEscapeSequence $ Color.extensionColorConfig config
      _ -> ""

groupReadLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
groupReadLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
               READ -> Color.groupReadPermBitEscapeSequence $ Color.extensionColorConfig config
               _ -> ""

groupWriteLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
groupWriteLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
               WRITE -> Color.groupWritePermBitEscapeSequence $ Color.extensionColorConfig config
               _ -> ""

groupExecLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
groupExecLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
      E_SETGID -> Color.setguiEscapeSequence config
      SETGID -> Color.setguiEscapeSequence config
      EXEC -> Color.groupExecPermBitEscapeSequence $ Color.extensionColorConfig config
      _ -> ""

otherReadLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
otherReadLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
               READ -> Color.otherReadPermBitEscapeSequence $ Color.extensionColorConfig config
               _ -> ""

otherWriteLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
otherWriteLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
               WRITE -> Color.otherWritePermBitEscapeSequence $ Color.extensionColorConfig config
               _ -> ""

otherExecLetterWithColor :: Color.Config -> FilemodeBitPatternType -> YAString.WrapedString
otherExecLetterWithColor config ptn = YAString.WrapedString
  { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSeq
  , YAString.wrappedStringMain = [letter]
  , YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
  }
  where
    letter = filemodeBitPatternTypeLetter ptn
    escSeq = case ptn of
      E_STICKY -> Color.stickyEscapeSequence config
      STICKY -> Color.stickyEscapeSequence config
      EXEC -> Color.otherExecPermBitEscapeSequence $ Color.extensionColorConfig config
      _ -> ""

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

fileTypeEscapeSequence :: Color.Config -> FileType -> String
fileTypeEscapeSequence config fType =
  case fType of
    REGULAR -> Color.fileEscaseSequence config
    BLOCK -> Color.blockDeviceEscapeSequence config
    CHAR -> Color.charDeviceEscapeSequence config
    DIR -> Color.directoryEscapeSequence config
    SYMLINK -> if symlinkEscSeq == "target" then "" else symlinkEscSeq
    FIFO -> Color.pipeEscapeSequence config
    SOCK -> Color.socketEscapeSequence config
    OTHER -> ""
    where
      symlinkEscSeq = Color.symlinkEscapeSequence config
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
