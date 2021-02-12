{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.Entry
  ( Entry (..),
    EntryType (..),
    Files (..),
    buildFiles,
    toEntries,
  )
where

import qualified Control.Monad as Monad
import qualified Control.Monad.Extra as Monad
import qualified Data.Either as E
import qualified Data.Functor as F
import qualified Data.List as List
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Sort as Sort
import qualified Haskellorls.Tree as Tree
import qualified Haskellorls.Utils as Utils
import qualified System.Directory as Directory
import qualified System.FilePath.Glob as Glob
import qualified System.FilePath.Posix as Posix
import qualified System.Posix.Files as Files

data EntryType = FILES | SINGLEDIR | DIRECTORY

data Entry = Entry
  { entryType :: EntryType,
    entryPath :: FilePath,
    entryContents :: [FilePath]
  }

-- Files deriving steps
-- 1. Get path list from arguments
-- 2. Exclude no existence path
-- 3. Partition files and directories
--
-- Entry list deriving steps
-- 1. Get Files
-- 2. Determine whether or not adding block size summary header
--   - need also all arguments includeing no existence
-- 3. List entries
data Files = Files
  { noExistences :: [FilePath],
    fileEntry :: Entry,
    directoryEntries :: [Entry]
  }

toEntries :: Files -> [Entry]
toEntries (Files noExists fEntry@Entry {..} dEntries) = fEntry' <> dEntries'
  where
    fEntry' = [fEntry | not $ null entryContents]
    dEntries' = case dEntries of
      [d] | all null [noExists, entryContents] -> [d {entryType = SINGLEDIR}]
      _ -> dEntries

buildDirectoryEntry :: Option.Option -> FilePath -> IO Entry
buildDirectoryEntry opt path = do
  contents <- listContents opt path >>= sortContents opt path . excluder
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
  (noExistences, exists) <- partitionExistOrNotPathes paths
  psPairs <- traverse f exists
  let fPaths = map fst $ filter (not . g) psPairs
      dPaths = map fst $ filter g psPairs
  directoryEntries <- Monad.concatMapM (dirPathToDirEntriesRecursively opt) dPaths
  return $
    Files
      { fileEntry = Entry FILES "" fPaths,
        ..
      }
  where
    f path = do
      status <- Files.getSymbolicLinkStatus path
      return (path, status)
    g = Files.isDirectory . snd

dirPathToDirEntriesRecursively :: Option.Option -> FilePath -> IO [Entry]
dirPathToDirEntriesRecursively opt path = do
  entry <- buildDirectoryEntry opt path
  entries <- entryToDirectoryEntries opt entry
  pure $ entry : entries

entryToDirectoryEntries :: Option.Option -> Entry -> IO [Entry]
entryToDirectoryEntries opt Entry {..}
  | Option.recursive opt && not isDepthZero = do
    entries <- Monad.filterM isDirectory pathes >>= mapM (buildDirectoryEntry opt)
    recursiveEntries <- Monad.concatMapM (entryToDirectoryEntries opt') entries
    pure $ entries <> recursiveEntries
  | otherwise = pure []
  where
    opt' = opt {Option.level = Tree.decreaseDepth $ Option.level opt}
    pathes = map (entryPath Posix.</>) entryContents
    depth = Option.level opt
    isDepthZero = (Just 0 ==) $ Tree.getDepth depth

isDirectory :: FilePath -> IO Bool
isDirectory path = Files.isDirectory <$> Files.getSymbolicLinkStatus path

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

sortContents :: Option.Option -> FilePath -> [FilePath] -> IO [FilePath]
sortContents opt path pathes = mapM (Node.nodeInfo path) pathes F.<&> map Node.nodeInfoPath . Sort.sorter opt

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

partitionExistOrNotPathes :: [FilePath] -> IO ([FilePath], [FilePath])
partitionExistOrNotPathes pathes = E.partitionEithers <$> mapM validatePathExistence pathes

validatePathExistence :: FilePath -> IO (Either FilePath FilePath)
validatePathExistence path = (\b -> if b then Right path else Left path) <$> Utils.exist path
