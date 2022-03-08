module Haskellorls.Exception
  ( printErr,
    module Control.Exception.Safe,
  )
where

import Control.Exception.Safe
import qualified System.IO as IO

-- | Print a message to stderr. This is similar to a shell command
-- `echo message >&2`.
printAsStderr :: (Show a) => a -> IO ()
printAsStderr = IO.hPrint IO.stderr

-- | Print an exception.
printErr :: Exception e => e -> IO ()
printErr = printAsStderr
