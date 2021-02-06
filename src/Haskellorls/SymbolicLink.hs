{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.SymbolicLink
  ( linkName,
    linkNameWrapper,
    coloredLinkName,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Color as Color
import qualified Haskellorls.Name as Name
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.WrappedText as WT

linkName :: Node.NodeInfo -> T.Text
linkName node = case node of
  Node.FileInfo {} -> ""
  _ -> prefix <> T.pack (Node.getDestPath node)

linkNameWrapper :: Node.NodeInfo -> [WT.WrappedText]
linkNameWrapper node
  | T.null link = []
  | otherwise = WT.toWrappedTextSingleton link
  where
    link = linkName node

coloredLinkName :: Color.Config -> Node.NodeInfo -> [WT.WrappedText]
coloredLinkName config node = case node of
  Node.FileInfo {} -> []
  _ -> prefix' <> output
  where
    output = Name.colorizedNodeName config $ Node.toFileInfo node
    prefix' = WT.toWrappedTextSingleton prefix

prefix :: T.Text
prefix = " -> "
