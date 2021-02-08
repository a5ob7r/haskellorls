module Haskellorls.Utils
  ( exist,
    linked,
  )
where

import qualified Control.Exception.Base as Exception
import qualified Data.Either as Either
import qualified System.Posix.Files as Files

exist :: FilePath -> IO Bool
exist path = Either.isRight <$> exist'
  where
    exist' :: IO (Either Exception.IOException Files.FileStatus)
    exist' = Exception.try $ Files.getSymbolicLinkStatus path

linked :: FilePath -> IO Bool
linked path = Either.isRight <$> exist'
  where
    exist' :: IO (Either Exception.IOException Files.FileStatus)
    exist' = Exception.try $ Files.getFileStatus path
