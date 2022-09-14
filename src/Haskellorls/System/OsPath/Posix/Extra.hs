module Haskellorls.System.OsPath.Posix.Extra
  ( decode,
    encode,
    module System.OsPath.Posix,
  )
where

import System.IO.Unsafe (unsafePerformIO)
import System.OsPath.Posix

encode :: FilePath -> PosixPath
encode = unsafePerformIO . encodeFS

decode :: PosixPath -> FilePath
decode = unsafePerformIO . decodeFS
