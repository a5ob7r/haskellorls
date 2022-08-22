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

  blockSize <- optional $ getEnv "LS_BLOCK_SIZE" <|> getEnv "BLOCK_SIZE"

  quotingStyle <- lookupEnv "QUOTING_STYLE"

  cwd <- getWorkingDirectory

  hostname <- getHostName

  columnSize <-
    let f = \case
          Just s | [(n, "")] <- readDec s -> Just n
          _ -> Nothing
     in liftA2 (<|>) (fmap width <$> size) $ f <$> lookupEnv "COLUMNS"

  return $ Environment {..}
