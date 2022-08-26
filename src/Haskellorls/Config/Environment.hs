module Haskellorls.Config.Environment
  ( Environment (..),
    mkEnvironment,
  )
where

import Control.Applicative
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Gettext (Catalog, loadCatalog)
import Haskellorls.Data.Gettext.Extra (lookupMO)
import Network.HostName
import Numeric
import System.Console.Terminal.Size
import System.Environment
import System.FilePath.Posix.ByteString (RawFilePath, decodeFilePath)
import System.IO
import System.Locale.SetLocale (Category (LC_MESSAGES), setLocale)
import System.Posix.Directory.ByteString

data Environment = Environment
  { toTerminal :: Bool,
    blockSize :: Maybe String,
    quotingStyle :: Maybe String,
    timeStyle :: Maybe String,
    cwd :: RawFilePath,
    hostname :: String,
    columnSize :: Maybe Int,
    lcMessages :: Maybe String,
    catalog :: Maybe Catalog
  }

mkEnvironment :: IO Environment
mkEnvironment = do
  toTerminal <- hIsTerminalDevice stdout

  blockSize <- optional $ getEnv "LS_BLOCK_SIZE" <|> getEnv "BLOCK_SIZE"

  quotingStyle <- lookupEnv "QUOTING_STYLE"

  timeStyle <- lookupEnv "TIME_STYLE"

  cwd <- getWorkingDirectory

  hostname <- getHostName

  columnSize <-
    let f = \case
          Just s | [(n, "")] <- readDec s -> Just n
          _ -> Nothing
     in runMaybeT $ width <$> MaybeT size <|> MaybeT (f <$> lookupEnv "COLUMNS")

  lcMessages <- setLocale LC_MESSAGES Nothing

  catalog <- runMaybeT do
    locale <- MaybeT $ return lcMessages
    path <- MaybeT $ lookupMO "/usr/share/locale" locale "coreutils"
    MaybeT . optional . loadCatalog $ decodeFilePath path

  return $ Environment {..}
