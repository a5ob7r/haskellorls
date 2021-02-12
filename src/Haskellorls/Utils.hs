module Haskellorls.Utils
  ( exist,
    linked,
    outputNoExistPathErr,
  )
where

import qualified Control.Exception.Base as Exception
import qualified Data.Either as Either
import qualified System.IO as IO
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

outputNoExistPathErr :: FilePath -> IO ()
outputNoExistPathErr path = IO.hPutStrLn IO.stderr $ "haskellorls: does not exist '" <> path <> "': (No such file or directory)"
