module Haskellorls.Utils
  ( getSymbolicLinkStatus,
    getFileStatus,
    destFileStatus,
    listContents,
    exclude,
    partitionExistOrNotPathes,
    validatePathExistence,
    isDirectory,
  )
where

import qualified Control.Exception.Base as Exception
import qualified Data.Either as E
import qualified Data.Either.Extra as E
import qualified Data.List as L
import qualified Haskellorls.Option as Option
import qualified System.Directory as Directory
import qualified System.FilePath.Glob as Glob
import qualified System.Posix.Files as Files

getSymbolicLinkStatus :: FilePath -> IO (Either Exception.IOException Files.FileStatus)
getSymbolicLinkStatus path = Exception.try $ Files.getSymbolicLinkStatus path

getFileStatus :: FilePath -> IO (Either Exception.IOException Files.FileStatus)
getFileStatus path = Exception.try $ Files.getSymbolicLinkStatus path

destFileStatus :: FilePath -> IO (Maybe Files.FileStatus)
destFileStatus path = E.eitherToMaybe <$> getFileStatus path

listContents :: Option.Option -> FilePath -> IO (Either Exception.IOException [FilePath])
listContents opt path = E.mapRight (ignoreExcluder . hideExcluder . ignoreFilter) <$> list path
  where
    list
      | Option.all opt || Option.noneSortExtra opt = listAllEntries
      | Option.almostAll opt = listSemiAllEntries
      | otherwise = listEntries
    ignoreFilter =
      if Option.ignoreBackups opt
        then ignoreBackupsFilter
        else id
    ignoreExcluder = case Option.ignore opt of
      "" -> id
      ptn -> exclude ptn
    hideExcluder = case Option.hide opt of
      "" -> id
      _ | isShowHiddenEntries -> id
      ptn -> exclude ptn
    isShowHiddenEntries = Option.all opt || Option.almostAll opt

listAllEntries :: FilePath -> IO (Either Exception.IOException [FilePath])
listAllEntries path = Exception.try $ Directory.getDirectoryContents path

listSemiAllEntries :: FilePath -> IO (Either Exception.IOException [FilePath])
listSemiAllEntries path = Exception.try $ Directory.listDirectory path

listEntries :: FilePath -> IO (Either Exception.IOException [FilePath])
listEntries path = E.mapRight (filter $ not . isHiddenEntries) <$> listSemiAllEntries path

isHiddenEntries :: FilePath -> Bool
isHiddenEntries [] = False
isHiddenEntries ('.' : _) = True
isHiddenEntries _ = False

ignoreBackupsFilter :: [FilePath] -> [FilePath]
ignoreBackupsFilter = filter (\path -> not $ "~" `L.isSuffixOf` path)

exclude :: String -> [FilePath] -> [FilePath]
exclude ptn = filter (not . Glob.match ptn')
  where
    ptn' = Glob.compile ptn

partitionExistOrNotPathes :: [FilePath] -> IO ([FilePath], [FilePath])
partitionExistOrNotPathes pathes = E.partitionEithers <$> mapM validatePathExistence pathes

validatePathExistence :: FilePath -> IO (Either FilePath FilePath)
validatePathExistence path = E.either (const $ Left path) (const $ Right path) <$> getSymbolicLinkStatus path

isDirectory :: FilePath -> IO Bool
isDirectory path = Files.isDirectory <$> Files.getSymbolicLinkStatus path
