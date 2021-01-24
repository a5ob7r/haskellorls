module Haskellorls.Entry
  ( listContents
  ) where

import qualified Haskellorls.Option as Option
import qualified System.Directory as Directory

listContents :: Option.Option -> FilePath -> IO [FilePath]
listContents opt
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
isHiddenEntries ('.':_) = True
isHiddenEntries _ = False
