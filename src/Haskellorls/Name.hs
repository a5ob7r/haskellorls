module Haskellorls.Name
  ( colorizedNodeName,
    nodeName,
  )
where

import qualified Haskellorls.Color as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.YetAnotherString as YAString
import qualified System.Posix.Files as Files
  ( FileStatus,
    fileMode,
    groupExecuteMode,
    intersectFileModes,
    isBlockDevice,
    isCharacterDevice,
    isDirectory,
    isNamedPipe,
    isRegularFile,
    isSocket,
    isSymbolicLink,
    otherExecuteMode,
    otherWriteMode,
    ownerExecuteMode,
    setGroupIDMode,
    setUserIDMode,
    unionFileModes,
  )
import qualified System.Posix.Types as Types (FileMode)

data NodeType
  = Directory
  | SymbolicLink
  | NamedPipe
  | Socket
  | BlockDevise
  | CharDevise
  | DoorsDevise -- NOTE: Doors device is not implemented on Linux
  | Setuid
  | Setgid
  | Sticky
  | StickyOtherWritable
  | OtherWritable
  | Executable
  | File
  | Orphan
  deriving (Show)

nodeTypeOf :: Files.FileStatus -> NodeType
nodeTypeOf status
  | Files.isRegularFile status = regularFileNodeTypeOf status
  | Files.isDirectory status = directoryNodeTypeOf status
  | Files.isSymbolicLink status = SymbolicLink
  | Files.isNamedPipe status = NamedPipe
  | Files.isSocket status = Socket
  | Files.isBlockDevice status = BlockDevise
  | Files.isCharacterDevice status = CharDevise
  | otherwise = Orphan

regularFileNodeTypeOf :: Files.FileStatus -> NodeType
regularFileNodeTypeOf status
  | isSetuidMode mode = Setuid
  | isSetgidMode mode = Setgid
  | isExecutableMode mode = Executable
  | otherwise = File
  where
    mode = Files.fileMode status

directoryNodeTypeOf :: Files.FileStatus -> NodeType
directoryNodeTypeOf status
  | isStickyOtherWrite mode = StickyOtherWritable
  | isOtherWriteMode mode = OtherWritable
  | isStickyMode mode = Sticky
  | otherwise = Directory
  where
    mode = Files.fileMode status

colorizedNodeName :: Color.Config -> Node.NodeInfo -> [YAString.WrapedString]
colorizedNodeName config nd =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config escSec,
        YAString.wrappedStringMain = name,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    name = nodeName nd
    escSec = lookupEscSec config nd

lookupEscSec :: Color.Config -> Node.NodeInfo -> String
lookupEscSec conf nd = case nodeTypeOf $ Node.nodeInfoStatus nd of
  Directory -> Color.directoryEscapeSequence conf
  SymbolicLink -> symlinkEscSeq
  NamedPipe -> Color.pipeEscapeSequence conf
  Socket -> Color.socketEscapeSequence conf
  BlockDevise -> Color.blockDeviceEscapeSequence conf
  CharDevise -> Color.charDeviceEscapeSequence conf
  DoorsDevise -> Color.doorEscapeSequence conf
  Setuid -> Color.setuidEscapeSequence conf
  Setgid -> Color.setguiEscapeSequence conf
  Sticky -> Color.stickyEscapeSequence conf
  StickyOtherWritable -> Color.stickyOtherWritableEscapeSequence conf
  OtherWritable -> Color.otherWritableEscapeSequence conf
  Executable -> Color.executableEscapeSequence conf
  File -> Color.lookupFilenameEscSec (Color.fileColorIndicator conf) $ nodeName nd
  Orphan -> orphanedSymlinkEscSeq
  where
    orphanedSymlinkEscSeq = Color.orphanedSymlinkEscapeSequence conf
    symlinkEscSeq = case nd of
      Node.FileInfo {} -> lookupSymlinkEscSeq
      Node.LinkInfo {} -> lookupSymlinkEscSeq
      Node.OrphanedLinkInfo {} -> orphanedSymlinkEscSeq
    lookupSymlinkEscSeq =
      if symlinkEscSeq' == "target"
        then
          lookupEscSec conf $
            Node.FileInfo
              { Node.getFilePath = Node.getDestPath nd,
                Node.getFileStatus = Node.getDestStatus nd
              }
        else symlinkEscSeq'
    symlinkEscSeq' = Color.symlinkEscapeSequence conf

nodeName :: Node.NodeInfo -> FilePath
nodeName = Node.nodeInfoPath

hasFileMode :: Types.FileMode -> Types.FileMode -> Bool
hasFileMode x y = x == Files.intersectFileModes x y

isOwnerExecuteMode :: Types.FileMode -> Bool
isOwnerExecuteMode = hasFileMode Files.ownerExecuteMode

isGroupExecuteMode :: Types.FileMode -> Bool
isGroupExecuteMode = hasFileMode Files.groupExecuteMode

isOtherWriteMode :: Types.FileMode -> Bool
isOtherWriteMode = hasFileMode Files.otherWriteMode

isOtherExecuteMode :: Types.FileMode -> Bool
isOtherExecuteMode = hasFileMode Files.otherExecuteMode

isExecutableMode :: Types.FileMode -> Bool
isExecutableMode = or . sequence [isOwnerExecuteMode, isGroupExecuteMode, isOtherExecuteMode]

isSetuidMode :: Types.FileMode -> Bool
isSetuidMode = hasFileMode Files.setUserIDMode

isSetgidMode :: Types.FileMode -> Bool
isSetgidMode = hasFileMode Files.setGroupIDMode

isStickyMode :: Types.FileMode -> Bool
isStickyMode = hasFileMode stickyMode

isStickyOtherWrite :: Types.FileMode -> Bool
isStickyOtherWrite = hasFileMode stickyOtherWriteMode

stickyMode :: Types.FileMode
stickyMode = 548

stickyOtherWriteMode :: Types.FileMode
stickyOtherWriteMode = Files.unionFileModes stickyMode Files.otherWriteMode
