-- | "Control.Exception.Safe" with extras.
module Haskellorls.Control.Exception.Safe
  ( printErr,
    module Control.Exception.Safe,
  )
where

import Control.Exception.Safe
import Control.Monad.IO.Class
import System.IO

-- | Print a message to stderr. This is similar to a shell command
-- @echo message >&2@.
printAsStderr :: (Show a, MonadIO m) => a -> m ()
printAsStderr = liftIO . hPrint stderr

-- | Print an exception.
printErr :: (Exception e, MonadIO m) => e -> m ()
printErr = printAsStderr
