module Haskellorls.Formatter.Filemode
  ( showFilemodeField,
    showFilemodeFieldWithNormalColor,
    showFilemodeFieldWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Filemode
import qualified Haskellorls.Formatter.Attribute as Attr
import Haskellorls.Formatter.Filemode.Entry
import Haskellorls.Formatter.Filemode.Permission
import qualified Haskellorls.Formatter.WrappedText as WT
import Haskellorls.LsColor
import Prelude hiding (lookup)

showFilemodeField :: Filemode -> [Attr.Attribute WT.WrappedText]
showFilemodeField (Filemode {..}) = [Attr.Other . WT.deserialize . T.pack $ fType : permFields]
  where
    fType = from getFiletype
    permFields = from <$> [getUserRead, getUserWrite, getUserExec, getGroupRead, getGroupWrite, getGroupExec, getOtherRead, getOtherWrite, getOtherExec]

-- | A node filemode field formatter for the @no@ parameter of the @LS_COLORS@.
showFilemodeFieldWithNormalColor :: LsColors -> Filemode -> [Attr.Attribute WT.WrappedText]
showFilemodeFieldWithNormalColor lscolors (Filemode {..}) = [Attr.Other $ WT.wrap lscolors normal field]
  where
    field = T.pack $ fType : permFields
    fType = from getFiletype
    permFields = from <$> [getUserRead, getUserWrite, getUserExec, getGroupRead, getGroupWrite, getGroupExec, getOtherRead, getOtherWrite, getOtherExec]

showFilemodeFieldWithColor :: LsColors -> Filemode -> [Attr.Attribute WT.WrappedText]
showFilemodeFieldWithColor lscolors (Filemode {..}) =
  Attr.Other
    <$> [ filetypeLetterWithColor lscolors getFiletype,
          userLetterWithColor lscolors getUserRead,
          userLetterWithColor lscolors getUserWrite,
          userLetterWithColor lscolors getUserExec,
          groupLetterWithColor lscolors getGroupRead,
          groupLetterWithColor lscolors getGroupWrite,
          groupLetterWithColor lscolors getGroupExec,
          otherLetterWithColor lscolors getOtherRead,
          otherLetterWithColor lscolors getOtherWrite,
          otherLetterWithColor lscolors getOtherExec
        ]
