{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Name.Decorator
  ( colorizedNodeName,
    colorizedNodeNameWrapper,
    nodeNameWrapper,
    nodeName,
    nodeTypeOf,
    lookupEscSeq,
    module Haskellorls.Name.Type,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.LsColor.Util as Color
import Haskellorls.Name.Type
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

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

colorizedNodeNameWrapper :: Option.Option -> Color.Config -> Node.NodeInfo -> [WT.WrappedText]
colorizedNodeNameWrapper opt config nd = Quote.quote (Quote.quoteStyle opt) $ colorizedNodeName opt config nd

colorizedNodeName :: Option.Option -> Color.Config -> Node.NodeInfo -> WT.WrappedText
colorizedNodeName opt config nd = Color.toWrappedText config getter $ Utils.escapeFormatter opt name
  where
    name = nodeName nd
    getter = lookupEscSeq nd

lookupEscSeq :: Node.NodeInfo -> Color.Config -> T.Text
lookupEscSeq nd conf = case nd of
  Node.FileInfo {} -> nd `lookupEscSeq'` conf
  Node.LinkInfo {} -> lookupSymlinkEscSeq
  Node.OrphanedLinkInfo {} -> Color.orphanedSymlinkEscapeSequence conf
  where
    lookupSymlinkEscSeq =
      if symlinkEscSeq' == "target"
        then Node.toFileInfo nd `lookupEscSeq'` conf
        else symlinkEscSeq'
    symlinkEscSeq' = Color.symlinkEscapeSequence conf

lookupEscSeq' :: Node.NodeInfo -> Color.Config -> T.Text
lookupEscSeq' nd conf = case nodeTypeOf $ Node.nodeInfoStatus nd of
  Directory -> Color.directoryEscapeSequence conf
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
  File -> nodeName nd `Color.lookupLsColor` Color.fileColorIndicator conf
  _ -> Color.orphanedSymlinkEscapeSequence conf

nodeNameWrapper :: Option.Option -> Node.NodeInfo -> [WT.WrappedText]
nodeNameWrapper opt node = Quote.quote style . WT.toWrappedText $ Utils.escapeFormatter opt name
  where
    name = nodeName node
    style = Quote.quoteStyle opt

nodeName :: Node.NodeInfo -> T.Text
nodeName = T.pack . Node.nodeInfoPath

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
