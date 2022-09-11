module Haskellorls.Formatter.Filemode
  ( showFilemodeField,
    showFilemodeFieldWithNormalColor,
    showFilemodeFieldWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Filemode
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import Haskellorls.LsColor
import Witch (from)
import Prelude hiding (lookup)

showFilemodeField :: Filemode -> [Attr.Attribute WT.WrappedText]
showFilemodeField (Filemode {..}) = [Attr.Other . from . T.pack $ fType : permFields]
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
    <$> [ filetypeLetter getFiletype,
          permLetter $ UserPerm getUserRead,
          permLetter $ UserPerm getUserWrite,
          permLetter $ UserPerm getUserExec,
          permLetter $ GroupPerm getGroupRead,
          permLetter $ GroupPerm getGroupWrite,
          permLetter $ GroupPerm getGroupExec,
          permLetter $ OtherPerm getOtherRead,
          permLetter $ OtherPerm getOtherWrite,
          permLetter $ OtherPerm getOtherExec
        ]
  where
    filetypeLetter entrytype = WT.wrap lscolors (lookup entrytype) . T.singleton $ from entrytype
    permLetter perm = WT.wrap lscolors (lookup perm) . T.singleton $ from perm
