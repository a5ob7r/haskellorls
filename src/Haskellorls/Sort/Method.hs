module Haskellorls.Sort.Method
  ( sorter,
  )
where

import qualified Algorithms.NaturalSort as NSort
import Data.Char
import qualified Data.List as L
import Data.Ord
import qualified Haskellorls.Format.Util as Format
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import Haskellorls.Sort.Type
import System.FilePath.Posix

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
  _ | Option.noneSortExtra opt -> sortWithNone
  NONE -> sortWithNone
  NAME
    | Option.noneSort opt -> sortWithNone
    | Option.sizeSort opt -> sortWithSize
    | Option.timeSort opt || ((Option.ctime opt || Option.atime opt) && not (Format.isLongStyle opt)) -> sorter' opt {Option.sort = TIME}
    | Option.naturalSort opt -> sortWithVersion
    | Option.extensionSort opt -> sortWithExtension
    | otherwise -> sortWithName
  SIZE -> sortWithSize
  TIME -> sortWithTime
  VERSION -> sortWithVersion
  EXTENSION -> sortWithExtension

order :: Option.Option -> [a] -> [a]
order opt
  | Option.reverse opt = reverse
  | otherwise = id

sortWithNone :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithNone = id

sortWithName :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithName = L.sortBy (\a b -> Node.getNodePath a `compareName` Node.getNodePath b)
  where
    compareName a b
      | a' == b' = a `compare` b
      | otherwise = a' `compare` b'
      where
        a' = norm a
        b' = norm b
    norm =
      map toUpper . \s -> case s of
        '.' : s' -> s'
        _ -> s

sortWithSize :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithSize = L.sortOn $ Down . Node.fileSize

sortWithTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithTime = L.sortOn $ Down . Node.fileTime

sortWithVersion :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithVersion = L.sortBy (\a b -> Node.getNodePath a `NSort.compare` Node.getNodePath b)

sortWithExtension :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithExtension = L.sortBy (\a b -> takeExtension (Node.getNodePath a) `compare` takeExtension (Node.getNodePath b))

partitionDirectoriesAndFiles :: [Node.NodeInfo] -> ([Node.NodeInfo], [Node.NodeInfo])
partitionDirectoriesAndFiles = L.partition isDirectory

-- For GNU ls compatibility about `--group-directories-first` option.
isDirectory :: Node.NodeInfo -> Bool
isDirectory = Node.isDirectory . Node.nodeType . Node.toFileInfo
