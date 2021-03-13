{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Name.Decorator
  ( colorizedNodeName,
    colorizedNodeNameWrapper,
    nodeNameWrapper,
    nodeName,
    lookupEscSeq,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.LsColor.Util as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Utils as Utils
import qualified Haskellorls.WrappedText as WT

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
lookupEscSeq' nd conf = case Node.pfsNodeType $ Node.nodeInfoStatus nd of
  Node.Directory -> Color.directoryEscapeSequence conf
  Node.NamedPipe -> Color.pipeEscapeSequence conf
  Node.Socket -> Color.socketEscapeSequence conf
  Node.BlockDevise -> Color.blockDeviceEscapeSequence conf
  Node.CharDevise -> Color.charDeviceEscapeSequence conf
  Node.DoorsDevise -> Color.doorEscapeSequence conf
  Node.Setuid -> Color.setuidEscapeSequence conf
  Node.Setgid -> Color.setguiEscapeSequence conf
  Node.Sticky -> Color.stickyEscapeSequence conf
  Node.StickyOtherWritable -> Color.stickyOtherWritableEscapeSequence conf
  Node.OtherWritable -> Color.otherWritableEscapeSequence conf
  Node.Executable -> Color.executableEscapeSequence conf
  Node.File -> nodeName nd `Color.lookupLsColor` Color.fileColorIndicator conf
  _ -> Color.orphanedSymlinkEscapeSequence conf

nodeNameWrapper :: Option.Option -> Node.NodeInfo -> [WT.WrappedText]
nodeNameWrapper opt node = Quote.quote style . WT.toWrappedText $ Utils.escapeFormatter opt name
  where
    name = nodeName node
    style = Quote.quoteStyle opt

nodeName :: Node.NodeInfo -> T.Text
nodeName = T.pack . Node.nodeInfoPath
