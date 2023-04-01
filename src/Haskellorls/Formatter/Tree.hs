module Haskellorls.Formatter.Tree
  ( treeBranch,
    treeBranchWithColor,
  )
where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Haskellorls.Config.Tree
import Haskellorls.Formatter.Attribute qualified as Attr
import Haskellorls.Formatter.WrappedText qualified as WT
import Haskellorls.LsColor qualified as Color

treeBranch :: [TreeNodePosition] -> T.Text
treeBranch [] = ""
treeBranch [x] = toTextOnLast x
treeBranch (x : xs) = TL.toStrict . TL.toLazyText $ L.foldl' (\acc a -> TL.fromText (toTextOnInit a) <> acc) (TL.fromText $ toTextOnLast x) xs

treeBranchWithColor :: Color.LsColors -> [TreeNodePosition] -> [Attr.Attribute WT.WrappedText]
treeBranchWithColor _ [] = []
treeBranchWithColor lscolors xs@(x : _) = [Attr.Other $ WT.wrap lscolors getter branch]
  where
    branch = treeBranch xs
    getter = Color.lookup x

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
