module Haskellorls.Config.Environment
  ( Environment (..),
    mkEnvironment,
  )
where

import Control.Applicative
import Network.HostName
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

  blockSize <- lookupEnv "LS_BLOCK_SIZE" <|> lookupEnv "BLOCK_SIZE"

  quotingStyle <- lookupEnv "QUOTING_STYLE"

  cwd <- getWorkingDirectory

  hostname <- getHostName

  columnSize <- fmap width <$> size

  return $ Environment {..}
