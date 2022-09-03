module Haskellorls.Config.Filemode.Entry
  ( EntryType (..),
    module Haskellorls.Class,
  )
where

import Haskellorls.Class
import Haskellorls.NodeInfo

-- TODO: some kinds of devices are not implemented.
data EntryType
  = REGULAR
  | BLOCK
  | CHAR
  | DIR
  | SYMLINK
  | FIFO
  | SOCK
  | OTHER
  deriving (Show)

instance From NodeInfo EntryType where
  from node = case nodeType node of
    Nothing -> SYMLINK
    Just SymbolicLink -> SYMLINK
    Just NamedPipe -> FIFO
    Just Socket -> SOCK
    Just BlockDevise -> BLOCK
    Just CharDevise -> CHAR
    nType
      | maybe False isDirectory nType -> DIR
      | otherwise -> REGULAR

instance From EntryType Char where
  from = \case
    REGULAR -> '-'
    BLOCK -> 'b'
    CHAR -> 'c'
    DIR -> 'd'
    SYMLINK -> 'l'
    FIFO -> 'p'
    SOCK -> 's'
    OTHER -> '?'
