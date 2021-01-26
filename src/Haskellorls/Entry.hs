module Haskellorls.Entry
  ( Entry (..),
    EntryType (..),
    Files (..),
    buildFiles,
    toEntries,
  )
where

import qualified Haskellorls.Option as Option
import qualified System.Directory as Directory
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
    dEntry = head dEntries
    dEntries'
      | null dEntries = []
      | null contents && length dEntries == 1 = [dEntry { entryPath = "" }]
      | otherwise = dEntries

buildDirectoryEntries :: Option.Option -> FilePath -> IO Entry
buildDirectoryEntries opt path = do
  contents <- listContents opt path
  return $ Entry DIRECTORY path contents

buildFiles :: Option.Option -> [FilePath] -> IO Files
buildFiles opt paths = do
  psPairs <- mapM f paths
  let fPaths = map fst $ filter (not . g) psPairs
      dPaths = map fst $ filter g psPairs
  dEntries <- mapM (buildDirectoryEntries opt) dPaths
  return $ Files
    { fileEntry = Entry FILES "" fPaths,
      directoryEntries = dEntries
    }
  where
    f path = do
      status <- Files.getSymbolicLinkStatus path
      return (path, status)
    g = Files.isDirectory . snd

listContents :: Option.Option -> FilePath -> IO [FilePath]
listContents opt = list
  where
    list
      | Option.all opt = listAllEntries
      | Option.almostAll opt = listSemiAllEntries
      | otherwise = listEntries

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
