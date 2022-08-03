module Haskellorls.Formatter.Filemode
  ( showFilemodeField,
    showFilemodeFieldWithNormalColor,
    showFilemodeFieldWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Filemode
import Haskellorls.Formatter.Filemode.Entry
import Haskellorls.Formatter.Filemode.Permission
import qualified Haskellorls.Formatter.WrappedText as WT
import Haskellorls.LsColor
import Prelude hiding (lookup)

showFilemodeField :: Filemode -> [WT.WrappedText]
showFilemodeField (Filemode {..}) = [WT.deserialize . T.pack $ fType : permFields]
  where
    fType = from getFiletype
    permFields = from <$> [getUserRead, getUserWrite, getUserExec, getGroupRead, getGroupWrite, getGroupExec, getOtherRead, getOtherWrite, getOtherExec]

-- | A node filemode field formatter for the @no@ parameter of the @LS_COLORS@.
showFilemodeFieldWithNormalColor :: LsColors -> Filemode -> [WT.WrappedText]
showFilemodeFieldWithNormalColor lscolors (Filemode {..}) = [WT.wrap lscolors normal field]
  where
    field = T.pack $ fType : permFields
    fType = from getFiletype
    permFields = from <$> [getUserRead, getUserWrite, getUserExec, getGroupRead, getGroupWrite, getGroupExec, getOtherRead, getOtherWrite, getOtherExec]

showFilemodeFieldWithColor :: LsColors -> Filemode -> [WT.WrappedText]
showFilemodeFieldWithColor lscolors (Filemode {..}) =
  [ fType,
    userReadLetter,
    userWriteLetter,
    userExecLetter,
    groupReadLetter,
    groupWriteLetter,
    groupExecLetter,
    otherReadLetter,
    otherWriteLetter,
    otherExecLetter
  ]
  where
    fType = filetypeLetterWithColor lscolors getFiletype
    userReadLetter = userLetterWithColor lscolors getUserRead
    userWriteLetter = userLetterWithColor lscolors getUserWrite
    userExecLetter = userLetterWithColor lscolors getUserExec
    groupReadLetter = groupLetterWithColor lscolors getGroupRead
    groupWriteLetter = groupLetterWithColor lscolors getGroupWrite
    groupExecLetter = groupLetterWithColor lscolors getGroupExec
    otherReadLetter = otherLetterWithColor lscolors getOtherRead
    otherWriteLetter = otherLetterWithColor lscolors getOtherWrite
    otherExecLetter = otherLetterWithColor lscolors getOtherExec
