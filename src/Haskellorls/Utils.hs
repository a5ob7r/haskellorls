module Haskellorls.Utils
  ( exist,
  )
where

import qualified Control.Exception.Base as Exception
import qualified Data.Either as Either
import qualified System.Posix.Files as Files

exist :: FilePath -> IO Bool
exist path = Either.fromRight False <$> exist'
  where
    exist' :: IO (Either Exception.IOException Bool)
    exist' = Exception.try $ Files.fileExist path
