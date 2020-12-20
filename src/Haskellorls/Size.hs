module Haskellorls.Size
    ( rawFileSize
    ) where

import System.Posix.Files
    ( fileSize
    , FileStatus
    )

rawFileSize :: FileStatus -> String
rawFileSize = show . fileSize
