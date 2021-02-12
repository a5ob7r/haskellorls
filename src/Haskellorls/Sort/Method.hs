module Haskellorls.Sort.Method
  ( sorter,
  )
where

import qualified Algorithms.NaturalSort as NSort
import qualified Data.List as L
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import Haskellorls.Sort.Type
import qualified Haskellorls.Time.Type as Time
import qualified System.Posix.Files as Files

sorter :: Option.Option -> [Node.NodeInfo] -> [Node.NodeInfo]
sorter opt = order opt . sorter' opt

sorter' :: Option.Option -> [Node.NodeInfo] -> [Node.NodeInfo]
sorter' opt = case sort of
  NONE -> sortWithNone
  NAME
    | natural -> sortWithVersion
    | otherwise -> sortWithName
  SIZE -> sortWithSize
  TIME -> case time of
    Time.MODIFICATION -> sortWithModificationTime
    Time.ACCESS -> sortWithAccessTime
    Time.CHANGE -> sortWithChangeTime
  VERSION -> sortWithVersion
  EXTENSION -> sortWithExtension
  where
    sort = Option.sort opt
    time = Option.time opt
    natural = Option.naturalSort opt

order :: Option.Option -> [a] -> [a]
order opt
  | Option.reverse opt = reverse
  | otherwise = id

sortWithNone :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithNone = id

sortWithName :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithName = L.sortOn Node.nodeInfoPath

sortWithSize :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithSize = L.sortOn $ Files.fileSize . Node.nodeInfoStatus

sortWithModificationTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithModificationTime = L.sortOn $ Files.modificationTime . Node.nodeInfoStatus

sortWithAccessTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithAccessTime = L.sortOn $ Files.accessTime . Node.nodeInfoStatus

sortWithChangeTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithChangeTime = L.sortOn $ Files.statusChangeTime . Node.nodeInfoStatus

sortWithVersion :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithVersion = L.sortBy (\a b -> Node.nodeInfoPath a `NSort.compare` Node.nodeInfoPath b)

sortWithExtension :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithExtension = id
