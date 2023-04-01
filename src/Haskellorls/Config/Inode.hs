module Haskellorls.Config.Inode (Inode (..)) where

import System.Posix.Types qualified as Types

newtype Inode = Inode {unInode :: Types.FileID}
