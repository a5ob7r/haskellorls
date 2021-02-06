module Haskellorls.Entry
  ( Entry (..),
    EntryType (..),
    Files (..),
    buildFiles,
    toEntries,
  )
where

import qualified System.FilePath.Glob as Glob
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Utils as Utils
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.Posix.Files as Files

data EntryType = FILES | DIRECTORY

data Entry = Entry
  { entryType :: EntryType,
    entryPath :: FilePath,
    entryContents :: [FilePath]
  }

data Files = Files
  { fileEntry :: Entry,
    directoryEntries :: [Entry]
  }

toEntries :: Files -> [Entry]
toEntries (Files fEntry@(Entry _ _ contents) dEntries) = fEntry' ++ dEntries'
  where
    fEntry' = [fEntry | not $ null contents]
    dEntries'
      | null contents && length dEntries == 1 = [(head dEntries) {entryType = FILES}]
      | otherwise = dEntries

buildDirectoryEntries :: Option.Option -> FilePath -> IO Entry
buildDirectoryEntries opt path = do
  contents <- excluder <$> listContents opt path
  return $ Entry DIRECTORY path contents
  where
    excluder = ignoreExcluder . hideExcluder
    ignorePtn = Option.ignore opt
    hidePtn = Option.hide opt
    isShowHiddenEntries = Option.all opt || Option.almostAll opt
    ignoreExcluder
      | null ignorePtn = id
      | otherwise = exclude ignorePtn
    hideExcluder
      | null hidePtn || isShowHiddenEntries = id
      | otherwise = exclude hidePtn

buildFiles :: Option.Option -> [FilePath] -> IO Files
buildFiles opt paths = do
  paths' <- existenceFilter paths
  psPairs <- traverse f paths'
  let fPaths = map fst $ filter (not . g) psPairs
      dPaths = map fst $ filter g psPairs
  dEntries <- mapM (buildDirectoryEntries opt) dPaths
  return $
    Files
      { fileEntry = Entry FILES "" fPaths,
        directoryEntries = dEntries
      }
  where
    f path = do
      status <- Files.getSymbolicLinkStatus path
      return (path, status)
    g = Files.isDirectory . snd

listContents :: Option.Option -> FilePath -> IO [FilePath]
listContents opt path = ignoreFilter <$> list path
  where
    list
      | Option.all opt = listAllEntries
      | Option.almostAll opt = listSemiAllEntries
      | otherwise = listEntries
    ignoreFilter =
      if Option.ignoreBackups opt
        then ignoreBackupsFilter
        else id

listAllEntries :: FilePath -> IO [FilePath]
listAllEntries = Directory.getDirectoryContents

listSemiAllEntries :: FilePath -> IO [FilePath]
listSemiAllEntries = Directory.listDirectory

listEntries :: FilePath -> IO [FilePath]
listEntries = fmap (filter $ not . isHiddenEntries) . listSemiAllEntries

isHiddenEntries :: FilePath -> Bool
isHiddenEntries [] = False
isHiddenEntries ('.' : _) = True
isHiddenEntries _ = False

ignoreBackupsFilter :: [FilePath] -> [FilePath]
ignoreBackupsFilter = filter (\path -> not $ "~" `List.isSuffixOf` path)

exclude :: String -> [FilePath] -> [FilePath]
exclude ptn = filter (not . Glob.match ptn')
  where
    ptn' = Glob.compile ptn

existenceFilter :: [FilePath] -> IO [FilePath]
existenceFilter = Monad.filterM exist

exist :: FilePath -> IO Bool
exist path = do
  isExist <- Utils.exist path
  if isExist
    then return ()
    else IO.hPutStrLn IO.stderr $ "haskellorls: does not exist '" <> path <> "': (No such file or directory)"
  return isExist
