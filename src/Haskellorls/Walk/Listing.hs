module Haskellorls.Walk.Listing (listContents) where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe)
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Listing
import Haskellorls.System.OsPath.Posix.Extra (PosixPath, decode, isExtSeparator, unpack)
import System.Directory.OsPath (getDirectoryContents, listDirectory)
import System.FilePath.Glob (match)
import System.OsString.Internal.Types (OsString (..))

-- | List contents in a directory in accordance with @Config@.
listContents :: (MonadThrow m, MonadIO m) => Config.Config -> PosixPath -> m [PosixPath]
listContents config path = filter ((\s -> not $ isBackup s || isMatchIgnore s || isMatchHide s) . decode) <$> list path
  where
    list = case Config.listing config of
      -- Force hide @.@ and @..@ to avoid infinite loop when the tree option is
      -- enabled.
      All
        | Config.tree config -> listAlmostAllEntries
        | otherwise -> listAllEntries
      AlmostAll -> listAlmostAllEntries
      NoHidden -> listEntries
    isBackup s = Config.ignoreBackups config && "~" `isSuffixOf` s
    isMatchIgnore s = maybe False (`match` s) $ Config.ignore config
    isMatchHide s = case Config.hide config of
      Just ptn | NoHidden <- Config.listing config -> ptn `match` s
      _ -> False

-- | Return all of contents in a directory. They contains all of normal files
-- and all of hidden files include @.@ and @..@.
listAllEntries :: (MonadIO m) => PosixPath -> m [PosixPath]
listAllEntries = fmap (getOsString <$>) . liftIO . getDirectoryContents . OsString

-- | Return almost all of contents in a directory. They contains all of normal
-- files and all of hidden files except @.@ and @..@.
listAlmostAllEntries :: (MonadIO m) => PosixPath -> m [PosixPath]
listAlmostAllEntries = fmap (getOsString <$>) . liftIO . listDirectory . OsString

-- | Return contents in a directory. They contains only all of normal files.
listEntries :: (MonadIO m) => PosixPath -> m [PosixPath]
listEntries path = filter (maybe True (not . isExtSeparator) . listToMaybe . unpack) <$> listAlmostAllEntries path
