module Haskellorls.Sort (sorter) where

import qualified Algorithms.NaturalSort as NSort
import qualified Data.List as L
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Sort
import qualified Haskellorls.NodeInfo as Node
import System.FilePath.Posix.ByteString

sorter :: Config.Config -> [Node.NodeInfo] -> [Node.NodeInfo]
sorter config = merger . separater . sorter' config
  where
    merger (dirs, files) = order config dirs <> order config files
    separater =
      if Config.groupDirectoriesFirst config && Config.sort config /= NONE
        then partitionDirectoriesAndFiles
        else ([],)

sorter' :: Config.Config -> [Node.NodeInfo] -> [Node.NodeInfo]
sorter' config = case Config.sort config of
  NONE -> sortWithNone
  NAME -> sortWithName
  SIZE -> sortWithSize
  TIME -> sortWithTime
  VERSION -> sortWithVersion
  EXTENSION -> sortWithExtension

order :: Config.Config -> [a] -> [a]
order config
  | Config.reverse config = reverse
  | otherwise = id

sortWithNone :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithNone = id

sortWithName :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithName = L.sortBy (\a b -> toPath a `compareName` toPath b)
  where
    toPath = T.decodeUtf8 . Node.getNodePath
    compareName a b
      | a' == b' = a `compare` b
      | otherwise = a' `compare` b'
      where
        a' = norm a
        b' = norm b
    norm =
      T.toUpper . \s -> case T.uncons s of
        Just ('.', s') -> s'
        _ -> s

sortWithSize :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithSize = L.sortOn $ Down . Node.fileSize

sortWithTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithTime = L.sortOn $ Down . Node.fileTime

sortWithVersion :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithVersion = L.sortBy (\a b -> toPath a `NSort.compare` toPath b)
  where
    toPath = T.decodeUtf8 . Node.getNodePath

sortWithExtension :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithExtension = L.sortBy (\a b -> takeExtension (Node.getNodePath a) `compare` takeExtension (Node.getNodePath b))

partitionDirectoriesAndFiles :: [Node.NodeInfo] -> ([Node.NodeInfo], [Node.NodeInfo])
partitionDirectoriesAndFiles = L.partition isDirectory

-- For GNU ls compatibility about `--group-directories-first` option.
isDirectory :: Node.NodeInfo -> Bool
isDirectory = Node.isDirectory . Node.nodeType . Node.toFileInfo
