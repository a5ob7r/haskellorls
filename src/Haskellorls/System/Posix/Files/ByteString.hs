-- | "System.Posix.Files.ByteString" with extras.
module Haskellorls.System.Posix.Files.ByteString
  ( stickyMode,
    hasFileMode,
    hasFileModesAll,
    hasFileModesOr,
    major,
    minor,
    module System.Posix.Files.ByteString,
  )
where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.List (foldl')
import System.Posix.Files.ByteString
import System.Posix.Types (DeviceID, FileMode)

stickyMode :: FileMode
stickyMode = 548

-- | Whether or not a 'FileMode' contains the 'FileMode'.
hasFileMode :: FileMode -> FileMode -> Bool
hasFileMode a mode = hasFileModesAll a [mode]

-- | Whether or not a 'FileMode' contains all of these 'FileMode's.
hasFileModesAll :: FileMode -> [FileMode] -> Bool
hasFileModesAll a modes = let mode = foldl' (.|.) 0 modes in a .&. mode == mode

-- | Whether or not a 'FileMode' contains at least one of these 'FileMode's.
hasFileModesOr :: FileMode -> [FileMode] -> Bool
hasFileModesOr a modes = let mode = foldl' (.|.) 0 modes in a .&. mode /= nullFileMode

-- | A major number of a device.
major :: DeviceID -> DeviceID
major did = (did .&. 0x00000000000fff00) `shiftR` 8 .|. (did .&. 0xfffff00000000000) `shiftR` 32

-- | A minor number of a device.
minor :: DeviceID -> DeviceID
minor did = (did .&. 0x00000000000000ff) `shiftR` 0 .|. (did .&. 0x00000ffffff00000) `shiftR` 12
