{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Haskellorls.Sort.Method
  ( sorter,
  )
where

import qualified Algorithms.NaturalSort as NSort
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Ord as O
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import Haskellorls.Sort.Type
import qualified Haskellorls.Time.Type as Time
import qualified System.FilePath.Posix as Posix
import qualified System.Posix.Files as Files

sorter :: Option.Option -> [Node.NodeInfo] -> [Node.NodeInfo]
sorter opt = merger . separater . sorter' opt
  where
    merger (dirs, files) = order opt dirs <> order opt files
    separater =
      if Option.groupDirectoriesFirst opt && not (Option.noneSort opt)
        then partitionDirectoriesAndFiles
        else ([],)

sorter' :: Option.Option -> [Node.NodeInfo] -> [Node.NodeInfo]
sorter' opt = case Option.sort opt of
  NONE -> sortWithNone
  NAME
    | Option.noneSort opt -> sortWithNone
    | Option.sizeSort opt -> sortWithSize
    | Option.timeSort opt -> sorter' opt {Option.sort = TIME}
    | Option.naturalSort opt -> sortWithVersion
    | Option.extensionSort opt -> sortWithExtension
    | otherwise -> sortWithName
  SIZE -> sortWithSize
  TIME -> case Option.time opt of
    Time.MODIFICATION -> sortWithModificationTime
    Time.ACCESS -> sortWithAccessTime
    Time.CHANGE -> sortWithChangeTime
  VERSION -> sortWithVersion
  EXTENSION -> sortWithExtension

order :: Option.Option -> [a] -> [a]
order opt
  | Option.reverse opt = reverse
  | otherwise = id

sortWithNone :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithNone = id

sortWithName :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithName = L.sortBy (\a b -> Node.nodeInfoPath a `compareName` Node.nodeInfoPath b)
  where
    compareName a b
      | a' == b' = a `compare` b
      | otherwise = a' `compare` b'
      where
        a' = norm a
        b' = norm b
    norm =
      map C.toUpper . \s -> case s of
        '.' : s' -> s'
        _ -> s

sortWithSize :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithSize = L.sortOn $ O.Down . Files.fileSize . Node.nodeInfoStatus

sortWithModificationTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithModificationTime = L.sortOn $ O.Down . Files.modificationTime . Node.nodeInfoStatus

sortWithAccessTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithAccessTime = L.sortOn $ O.Down . Files.accessTime . Node.nodeInfoStatus

sortWithChangeTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithChangeTime = L.sortOn $ O.Down . Files.statusChangeTime . Node.nodeInfoStatus

sortWithVersion :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithVersion = L.sortBy (\a b -> Node.nodeInfoPath a `NSort.compare` Node.nodeInfoPath b)

sortWithExtension :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithExtension = L.sortBy (\a b -> Posix.takeExtension (Node.nodeInfoPath a) `compare` Posix.takeExtension (Node.nodeInfoPath b))

partitionDirectoriesAndFiles :: [Node.NodeInfo] -> ([Node.NodeInfo], [Node.NodeInfo])
partitionDirectoriesAndFiles = L.partition isDirectory

-- For GNU ls compatibility about `--group-directories-first` option.
isDirectory :: Node.NodeInfo -> Bool
isDirectory node = Files.isDirectory status
  where
    status = case node of
      Node.LinkInfo {..} -> getDestStatus
      _ -> Node.nodeInfoStatus node
