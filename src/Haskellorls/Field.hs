{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Field
  ( FilemodeField (..),
    filemodeField,
    fileTypeLetter,
    filemodeBitPatternTypeLetter,
    showFilemodeField,
    showFilemodeFieldWithColor,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Color as Color
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

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
  { getFiletype :: FileType,
    getUserRead :: FilemodeBitPatternType,
    getUserWrite :: FilemodeBitPatternType,
    getUserExec :: FilemodeBitPatternType,
    getGroupRead :: FilemodeBitPatternType,
    getGroupWrite :: FilemodeBitPatternType,
    getGroupExec :: FilemodeBitPatternType,
    getOtherRead :: FilemodeBitPatternType,
    getOtherWrite :: FilemodeBitPatternType,
    getOtherExec :: FilemodeBitPatternType
  }

filemodeField :: Files.FileStatus -> FilemodeField
filemodeField status =
  FilemodeField
    { getFiletype = fileType status,
      getUserRead = userReadModeType mode,
      getUserWrite = userWriteModeType mode,
      getUserExec = userExecModeType mode,
      getGroupRead = groupReadModeType mode,
      getGroupWrite = groupWriteModeType mode,
      getGroupExec = groupExecModeType mode,
      getOtherRead = otherReadModeType mode,
      getOtherWrite = otherWriteModeType mode,
      getOtherExec = otherExecModeType mode
    }
  where
    mode = Files.fileMode status

showFilemodeField :: FilemodeField -> [WT.WrappedText]
showFilemodeField field = WT.toWrappedTextSingleton . T.pack $ fType : permFields
  where
    fType = fileTypeLetter $ getFiletype field
    permFields = map (\f -> filemodeBitPatternTypeLetter $ f field) permLetterFuncs

permLetterFuncs :: [FilemodeField -> FilemodeBitPatternType]
permLetterFuncs =
  [ getUserRead,
    getUserWrite,
    getUserExec,
    getGroupRead,
    getGroupWrite,
    getGroupExec,
    getOtherRead,
    getOtherWrite,
    getOtherExec
  ]

showFilemodeFieldWithColor :: Color.Config -> FilemodeField -> [WT.WrappedText]
showFilemodeFieldWithColor config field =
  [ fType,
    userReadLetter,
    userWriteLetter,
    userExecLetter,
    groupReadLetter,
    groupWriteLetter,
    groupExecLetter,
    otherReadLetter,
    otherWriteLetter,
    otherExecLetter
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

filetypeLetterWithColor :: Color.Config -> FileType -> WT.WrappedText
filetypeLetterWithColor config filetype = Color.toWrappedText config getter fType
  where
    fType = T.singleton $ fileTypeLetter filetype
    getter = flip fileTypeEscapeSequence filetype

userReadLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
userReadLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      READ -> Color.userReadPermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

userWriteLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
userWriteLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      WRITE -> Color.userWritePermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

userExecLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
userExecLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      E_SETUID -> Color.setuidEscapeSequence
      SETUID -> Color.setuidEscapeSequence
      EXEC -> Color.userExecPermBitFileEscapeSequence . Color.extensionColorConfig
      _ -> const ""

groupReadLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
groupReadLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      READ -> Color.groupReadPermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

groupWriteLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
groupWriteLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      WRITE -> Color.groupWritePermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

groupExecLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
groupExecLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      E_SETGID -> Color.setguiEscapeSequence
      SETGID -> Color.setguiEscapeSequence
      EXEC -> Color.groupExecPermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

otherReadLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
otherReadLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      READ -> Color.otherReadPermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

otherWriteLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
otherWriteLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      WRITE -> Color.otherWritePermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

otherExecLetterWithColor :: Color.Config -> FilemodeBitPatternType -> WT.WrappedText
otherExecLetterWithColor config ptn = Color.toWrappedText config getter letter
  where
    letter = T.singleton $ filemodeBitPatternTypeLetter ptn
    getter = case ptn of
      E_STICKY -> Color.stickyEscapeSequence
      STICKY -> Color.stickyEscapeSequence
      EXEC -> Color.otherExecPermBitEscapeSequence . Color.extensionColorConfig
      _ -> const ""

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
fileType :: Files.FileStatus -> FileType
fileType status
  | Files.isRegularFile status = REGULAR
  | Files.isDirectory status = DIR
  | Files.isBlockDevice status = BLOCK
  | Files.isCharacterDevice status = CHAR
  | Files.isSymbolicLink status = SYMLINK
  | Files.isNamedPipe status = FIFO
  | Files.isSocket status = SOCK
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

fileTypeEscapeSequence :: Color.Config -> FileType -> T.Text
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
userReadModeType :: Types.FileMode -> FilemodeBitPatternType
userReadModeType mode
  | hasUserReadMode mode = READ
  | otherwise = NOTHING

userWriteModeType :: Types.FileMode -> FilemodeBitPatternType
userWriteModeType mode
  | hasUserWriteMode mode = WRITE
  | otherwise = NOTHING

userExecModeType :: Types.FileMode -> FilemodeBitPatternType
userExecModeType mode
  | isSetuid && isExec = E_SETUID
  | isSetuid = SETUID
  | isExec = EXEC
  | otherwise = NOTHING
  where
    isExec = hasUserExecMode mode
    isSetuid = hasSetuidMode mode

groupReadModeType :: Types.FileMode -> FilemodeBitPatternType
groupReadModeType mode
  | hasGroupReadMode mode = READ
  | otherwise = NOTHING

groupWriteModeType :: Types.FileMode -> FilemodeBitPatternType
groupWriteModeType mode
  | hasGroupWriteMode mode = WRITE
  | otherwise = NOTHING

groupExecModeType :: Types.FileMode -> FilemodeBitPatternType
groupExecModeType mode
  | isSetgid && isExec = E_SETGID
  | isSetgid = SETGID
  | isExec = EXEC
  | otherwise = NOTHING
  where
    isExec = hasGroupExecMode mode
    isSetgid = hasSetgidMode mode

otherReadModeType :: Types.FileMode -> FilemodeBitPatternType
otherReadModeType mode
  | hasOtherReadMode mode = READ
  | otherwise = NOTHING

otherWriteModeType :: Types.FileMode -> FilemodeBitPatternType
otherWriteModeType mode
  | hasOtherWriteMode mode = WRITE
  | otherwise = NOTHING

otherExecModeType :: Types.FileMode -> FilemodeBitPatternType
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

-- }}}
