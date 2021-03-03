{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.SymbolicLink
  ( linkName,
    coloredLinkName,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.Name.Decorator as Name
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.WrappedText as WT

linkName :: Option.Option -> Node.NodeInfo -> [WT.WrappedText]
linkName opt node = case node of
  Node.FileInfo {} -> []
  _ -> prefix' <> Quote.quote style (WT.toWrappedText link)
  where
    style = Quote.quoteStyleForLink opt
    link = Name.nodeName $ Node.toFileInfo node
    prefix' = WT.toWrappedTextSingleton prefix

coloredLinkName :: Option.Option -> Color.Config -> Node.NodeInfo -> [WT.WrappedText]
coloredLinkName opt config node = case node of
  Node.FileInfo {} -> []
  _ -> prefix' <> Quote.quote style link
  where
    style = Quote.quoteStyleForLink opt
    link = Name.colorizedNodeName config $ Node.toFileInfo node
    prefix' = WT.toWrappedTextSingleton prefix

prefix :: T.Text
prefix = " -> "
