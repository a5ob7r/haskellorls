module Haskellorls.Tree.Decorator
  ( treeBranch,
    treeBranchWithColor,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as L
import qualified Haskellorls.LsColor as Color
import Haskellorls.Tree.Type
import qualified Haskellorls.WrappedText as WT

treeBranch :: [TreeNodePosition] -> T.Text
treeBranch [] = ""
treeBranch [x] = toTextOnLast x
treeBranch (x : xs) = L.toStrict . L.toLazyText $ foldr (\a acc -> L.fromText (toTextOnInit a) <> acc) (L.fromText $ toTextOnLast x) xs

treeBranchWithColor :: Color.LsColors -> [TreeNodePosition] -> [WT.WrappedText]
treeBranchWithColor _ [] = []
treeBranchWithColor lscolors xs@(x : _) = [Color.toWrappedText lscolors getter branch]
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
