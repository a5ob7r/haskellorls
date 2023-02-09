module Haskellorls.Config.Environment
  ( Environment (..),
    mkEnvironment,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Gettext (Catalog, loadCatalog)
import Haskellorls.Data.Gettext.Extra (lookupMO)
import qualified Haskellorls.System.Locale as Locale
import Network.HostName (getHostName)
import Numeric (readDec)
import System.Console.Terminal.Size (Window (width), size)
import System.Environment (getEnv, lookupEnv)
import System.FilePath.Posix.ByteString (RawFilePath, decodeFilePath)
import System.IO (hIsTerminalDevice, stdout)
import System.Posix.Directory.ByteString (getWorkingDirectory)
import Witch (from)

data Environment = Environment
  { toTerminal :: Bool,
    blockSize :: Maybe String,
    onlyBlockSize :: Maybe String,
    posixlyCorrect :: Maybe String,
    quotingStyle :: Maybe String,
    timeStyle :: Maybe String,
    cwd :: RawFilePath,
    hostname :: String,
    columnSize :: Maybe Int,
    lcMessages :: Locale.LcMessages,
    catalog :: Maybe Catalog
  }

mkEnvironment :: (MonadIO m) => m Environment
mkEnvironment = do
  toTerminal <- liftIO $ hIsTerminalDevice stdout

  blockSize <- liftIO . optional $ getEnv "LS_BLOCK_SIZE" <|> getEnv "BLOCK_SIZE"

  onlyBlockSize <- liftIO $ lookupEnv "BLOCKSIZE"

  posixlyCorrect <- liftIO $ lookupEnv "POSIXLY_CORRECT"

  quotingStyle <- liftIO $ lookupEnv "QUOTING_STYLE"

  timeStyle <- liftIO $ lookupEnv "TIME_STYLE"

  cwd <- liftIO getWorkingDirectory

  hostname <- liftIO getHostName

  columnSize <-
    let f = \case
          Just s | [(n, "")] <- readDec s -> Just n
          _ -> Nothing
     in runMaybeT $ width <$> MaybeT (liftIO size) <|> MaybeT (f <$> liftIO (lookupEnv "COLUMNS"))

  lcMessages <- Locale.lcMessages

  catalog <- runMaybeT do
    locale <- MaybeT . return $ from lcMessages
    path <- MaybeT . liftIO $ lookupMO "/usr/share/locale" locale "coreutils"
    MaybeT . liftIO . optional . loadCatalog $ decodeFilePath path

  return $ Environment {..}
