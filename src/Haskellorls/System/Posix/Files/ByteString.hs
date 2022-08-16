-- | "System.Posix.Files.ByteString" with extras.
module Haskellorls.System.Posix.Files.ByteString
  ( major,
    minor,
    module System.Posix.Files.ByteString,
  )
where

import Data.Bits
import System.Posix.Files.ByteString
import System.Posix.Types (DeviceID)

-- | A major number of a device.
major :: DeviceID -> DeviceID
major did = (did .&. 0x00000000000fff00) `shiftR` 8 .|. (did .&. 0xfffff00000000000) `shiftR` 32

-- | A minor number of a device.
minor :: DeviceID -> DeviceID
minor did = (did .&. 0x00000000000000ff) `shiftR` 0 .|. (did .&. 0x00000ffffff00000) `shiftR` 12
