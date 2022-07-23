module Haskellorls.Config.Inode (Inode (..)) where

import qualified System.Posix.Types as Types

newtype Inode = Inode {unInode :: Types.FileID}
