module Haskellorls.Utils
  ( destFileStatusRecursive,
    linkDestPath,
    listContents,
    exclude,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Functor
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Listing
import RawFilePath.Directory
import System.FilePath.Glob
import System.FilePath.Posix.ByteString
import qualified System.Posix.Files.ByteString as Files

-- | A useful version of 'getFileStatus', to get a resolved destination file's
-- status, but not a symbolic link self.
destFileStatusRecursive :: MonadIO m => RawFilePath -> RawFilePath -> m Files.FileStatus
destFileStatusRecursive dirPath basePath = do
  s <- liftIO . Files.getFileStatus $ linkDestPath dirPath basePath

  if Files.isSymbolicLink s
    then do
      link <- liftIO . Files.readSymbolicLink $ linkDestPath dirPath basePath
      destFileStatusRecursive dirPath link
    else pure s

-- | Create a symlink's filepath from symlink's parent directory path and a
-- filepath written in symlink self.
linkDestPath :: RawFilePath -> RawFilePath -> RawFilePath
linkDestPath parPath linkPath
  | isAbsolute linkPath = linkPath
  | otherwise = takeDirectory parPath </> linkPath

-- | List contents in a directory in accordance with @Config@.
listContents :: MonadIO m => Config.Config -> RawFilePath -> m [RawFilePath]
listContents config path = list path <&> ignoreExcluder . hideExcluder . ignoreFilter
  where
    list = case Config.listing config of
      All -> listAllEntries
      AlmostAll -> listSemiAllEntries
      NoHidden -> listEntries
    ignoreFilter =
      if Config.ignoreBackups config
        then ignoreBackupsFilter
        else id
    ignoreExcluder = case Config.ignore config of
      "" -> id
      ptn -> exclude ptn
    hideExcluder = case Config.hide config of
      "" -> id
      _ | isShowHiddenEntries -> id
      ptn -> exclude ptn
    isShowHiddenEntries = case Config.listing config of
      NoHidden -> False
      _ -> True

-- | Return all of contents in a directory. They contains all of normal files
-- and all of hidden files include @.@ and @..@.
listAllEntries :: MonadIO m => RawFilePath -> m [RawFilePath]
listAllEntries = liftIO . getDirectoryFiles

-- | Return almost all of contents in a directory. They contains all of normal
-- files and all of hidden files except @.@ and @..@.
listSemiAllEntries :: MonadIO m => RawFilePath -> m [RawFilePath]
listSemiAllEntries = liftIO . listDirectory

-- | Return contents in a directory. They contains only all of normal files.
listEntries :: MonadIO m => RawFilePath -> m [RawFilePath]
listEntries path = listSemiAllEntries path <&> filter (\s -> not $ "." `B.isPrefixOf` s)

-- | Exclude backup files, which have @~@ as a suffix.
ignoreBackupsFilter :: [RawFilePath] -> [RawFilePath]
ignoreBackupsFilter = filter (\s -> not $ "~" `B.isSuffixOf` s)

-- | TODO: Use "ByteString" native globbing instead of "String" one.
exclude :: String -> [RawFilePath] -> [RawFilePath]
exclude ptn = map encodeFilePath . filter (not . match ptn') . map decodeFilePath
  where
    ptn' = compile ptn
