module Haskellorls.Filemode.Entry.Type
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
  from node = case nType of
    SymbolicLink -> SYMLINK
    NamedPipe -> FIFO
    Socket -> SOCK
    BlockDevise -> BLOCK
    CharDevise -> CHAR
    _
      | isDirectory nType -> DIR
      | otherwise -> REGULAR
    where
      nType = nodeType node

instance From EntryType Char where
  from = \case
    REGULAR -> regularFileLetter
    BLOCK -> blockLetter
    CHAR -> charLetter
    DIR -> directoryLetter
    SYMLINK -> symlinkLetter
    FIFO -> fifoLetter
    SOCK -> socketLetter
    OTHER -> otherLetter

regularFileLetter :: Char
regularFileLetter = '-'

blockLetter :: Char
blockLetter = 'b'

charLetter :: Char
charLetter = 'c'

directoryLetter :: Char
directoryLetter = 'd'

symlinkLetter :: Char
symlinkLetter = 'l'

fifoLetter :: Char
fifoLetter = 'p'

socketLetter :: Char
socketLetter = 's'

otherLetter :: Char
otherLetter = '?'
