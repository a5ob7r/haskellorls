module Haskellorls.Config.Environment
  ( Environment (..),
    mkEnvironment,
  )
where

import Control.Applicative
import Network.HostName
import Numeric
import System.Console.Terminal.Size
import System.Environment
import System.FilePath.Posix.ByteString
import System.IO
import System.Posix.Directory.ByteString

data Environment = Environment
  { toTerminal :: Bool,
    blockSize :: Maybe String,
    quotingStyle :: Maybe String,
    cwd :: RawFilePath,
    hostname :: String,
    columnSize :: Maybe Int
  }

mkEnvironment :: IO Environment
mkEnvironment = do
  toTerminal <- hIsTerminalDevice stdout

  blockSize <- liftA2 (<|>) (lookupEnv "LS_BLOCK_SIZE") (lookupEnv "BLOCK_SIZE")

  quotingStyle <- lookupEnv "QUOTING_STYLE"

  cwd <- getWorkingDirectory

  hostname <- getHostName

  columnSize <-
    liftA2 (<|>) (fmap width <$> size) $
      lookupEnv "COLUMNS" >>= \e ->
        return $
          e >>= \cols -> case readDec cols of
            [(n, "")] -> Just n
            _ -> Nothing

  return $ Environment {..}
