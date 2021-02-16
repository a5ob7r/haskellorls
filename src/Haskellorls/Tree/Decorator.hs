{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Tree.Decorator
  ( treeBranch,
    treeBranchWithColor,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.LsColor.Config as Color
import Haskellorls.Tree.Type
import qualified Haskellorls.WrappedText as WT

treeBranch :: [TreeNodePosition] -> T.Text
treeBranch [] = ""
treeBranch [x] = toTextOnLast x
treeBranch (x : xs) = toTextOnInit x <> treeBranch xs

treeBranchWithColor :: Color.Config -> [TreeNodePosition] -> [WT.WrappedText]
treeBranchWithColor config xs = [Color.toWrappedText config getter $ treeBranch xs]
  where
    getter = Color.treeBranchEscapeSequence . Color.extensionColorConfig

toTextOnInit :: TreeNodePosition -> T.Text
toTextOnInit = \case
  HEAD -> "│  "
  MID -> "│  "
  LAST -> "   "

toTextOnLast :: TreeNodePosition -> T.Text
toTextOnLast = \case
  HEAD -> "├── "
  MID -> "├── "
  LAST -> "└── "
