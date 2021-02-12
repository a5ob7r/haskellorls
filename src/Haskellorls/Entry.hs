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

import qualified Data.Functor as F
import qualified Control.Monad as Monad
import qualified Control.Monad.Extra as Monad
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

data Files = Files
  { fileEntry :: Entry,
    directoryEntries :: [Entry]
  }

toEntries :: Files -> [Entry]
toEntries (Files fEntry@(Entry _ _ contents) dEntries) = fEntry' ++ dEntries'
  where
    fEntry' = [fEntry | not $ null contents]
    dEntries'
      | null contents && length dEntries == 1 = [(head dEntries) {entryType = SINGLEDIR}]
      | otherwise = dEntries

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
  paths' <- existenceFilter paths
  psPairs <- traverse f paths'
  let fPaths = map fst $ filter (not . g) psPairs
      dPaths = map fst $ filter g psPairs
  dEntries <- Monad.concatMapM (dirPathToDirEntriesRecursively opt) dPaths
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

existenceFilter :: [FilePath] -> IO [FilePath]
existenceFilter = Monad.filterM exist

exist :: FilePath -> IO Bool
exist path = do
  isExist <- Utils.exist path
  if isExist
    then return ()
    else Utils.outputNoExistPathErr path
  return isExist
