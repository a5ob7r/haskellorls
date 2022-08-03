module Haskellorls.Formatter.Filemode.Entry
  ( filetypeLetterWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Config.Filemode.Entry
import qualified Haskellorls.Formatter.WrappedText as WT
import Haskellorls.LsColor
import Prelude hiding (lookup)

filetypeLetterWithColor :: LsColors -> EntryType -> WT.WrappedText
filetypeLetterWithColor lscolors entrytype = WT.wrap lscolors getter fType
  where
    fType = T.singleton $ from entrytype
    getter = lookup entrytype
