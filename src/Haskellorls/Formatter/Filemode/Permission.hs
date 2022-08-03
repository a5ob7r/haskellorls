module Haskellorls.Formatter.Filemode.Permission
  ( userLetterWithColor,
    groupLetterWithColor,
    otherLetterWithColor,
    module Haskellorls.Config.Filemode.Permission,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Filemode.Permission
import qualified Haskellorls.Formatter.WrappedText as WT
import Haskellorls.LsColor
import Prelude hiding (lookup)

userLetterWithColor :: LsColors -> PermissionClass -> WT.WrappedText
userLetterWithColor lscolors c = letterWithColor lscolors $ UserPerm c

groupLetterWithColor :: LsColors -> PermissionClass -> WT.WrappedText
groupLetterWithColor lscolors c = letterWithColor lscolors $ GroupPerm c

otherLetterWithColor :: LsColors -> PermissionClass -> WT.WrappedText
otherLetterWithColor lscolors c = letterWithColor lscolors $ OtherPerm c

letterWithColor :: LsColors -> Permission -> WT.WrappedText
letterWithColor lscolors p = WT.wrap lscolors getter letter
  where
    letter = T.singleton $ from p
    getter = lookup p
