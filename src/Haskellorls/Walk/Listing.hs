module Haskellorls.Walk.Listing (listContents) where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Listing
import RawFilePath.Directory
import System.FilePath.Glob (Pattern, match)
import System.FilePath.Posix.ByteString

-- | List contents in a directory in accordance with @Config@.
listContents :: (MonadIO m) => Config.Config -> RawFilePath -> m [RawFilePath]
listContents config path = ignoreExcluder . hideExcluder . ignoreFilter <$> list path
  where
    list = case Config.listing config of
      All -> listAllEntries
      AlmostAll -> listSemiAllEntries
      NoHidden -> listEntries
    ignoreFilter =
      if Config.ignoreBackups config
        then ignoreBackupsFilter
        else id
    ignoreExcluder = maybe id exclude $ Config.ignore config
    hideExcluder = case Config.hide config of
      Just ptn | NoHidden <- Config.listing config -> exclude ptn
      _ -> id

-- | Return all of contents in a directory. They contains all of normal files
-- and all of hidden files include @.@ and @..@.
listAllEntries :: (MonadIO m) => RawFilePath -> m [RawFilePath]
listAllEntries = liftIO . getDirectoryFiles

-- | Return almost all of contents in a directory. They contains all of normal
-- files and all of hidden files except @.@ and @..@.
listSemiAllEntries :: (MonadIO m) => RawFilePath -> m [RawFilePath]
listSemiAllEntries = liftIO . listDirectory

-- | Return contents in a directory. They contains only all of normal files.
listEntries :: (MonadIO m) => RawFilePath -> m [RawFilePath]
listEntries path = filter (\s -> not $ "." `B.isPrefixOf` s) <$> listSemiAllEntries path

-- | Exclude backup files, which have @~@ as a suffix.
ignoreBackupsFilter :: [RawFilePath] -> [RawFilePath]
ignoreBackupsFilter = filter (\s -> not $ "~" `B.isSuffixOf` s)

-- | TODO: Use "ByteString" native globbing instead of "String" one.
exclude :: Pattern -> [RawFilePath] -> [RawFilePath]
exclude ptn = map encodeFilePath . filter (not . match ptn) . map decodeFilePath
