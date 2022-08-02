module Haskellorls.Formatter.Filemode.Permission
  ( userLetterWithColor,
    groupLetterWithColor,
    otherLetterWithColor,
    module Haskellorls.Config.Filemode.Permission,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Filemode.Permission
import Haskellorls.LsColor
import qualified Haskellorls.WrappedText as WT
import Prelude hiding (lookup)

userLetterWithColor :: LsColors -> PermissionClass -> WT.WrappedText
userLetterWithColor lscolors c = letterWithColor lscolors $ UserPerm c

groupLetterWithColor :: LsColors -> PermissionClass -> WT.WrappedText
groupLetterWithColor lscolors c = letterWithColor lscolors $ GroupPerm c

otherLetterWithColor :: LsColors -> PermissionClass -> WT.WrappedText
otherLetterWithColor lscolors c = letterWithColor lscolors $ OtherPerm c

letterWithColor :: LsColors -> Permission -> WT.WrappedText
letterWithColor lscolors p = toWrappedText lscolors getter letter
  where
    letter = T.singleton $ from p
    getter = lookup p