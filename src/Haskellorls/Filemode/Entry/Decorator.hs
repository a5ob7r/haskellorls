module Haskellorls.Filemode.Entry.Decorator
  ( filetypeLetterWithColor,
  )
where

import qualified Data.Text as T
import Haskellorls.Filemode.Entry.Type
import Haskellorls.LsColor
import qualified Haskellorls.WrappedText as WT
import Prelude hiding (lookup)

filetypeLetterWithColor :: LsColors -> EntryType -> WT.WrappedText
filetypeLetterWithColor lscolors entrytype = toWrappedText lscolors getter fType
  where
    fType = T.singleton $ from entrytype
    getter = lookup entrytype
