module Haskellorls.Sort
  ( sorter,
  )
where

import qualified Data.List as L
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Time.Type as Time
import qualified System.Posix.Files as Files

data SortType
  = NONE
  | NAME
  | SIZE
  | MODIFICATIONTIME
  | ACCESSTIME
  | CHANGETIME
  | VERSION
  | EXTENSION

sorter :: Option.Option -> [Node.NodeInfo] -> [Node.NodeInfo]
sorter opt = order opt . sortWith sortType
  where
    sortType = sortTypeFrom opt

order :: Option.Option -> [a] -> [a]
order opt
  | Option.reverse opt = reverse
  | otherwise = id

sortTypeFrom :: Option.Option -> SortType
sortTypeFrom opt
  | sort == "time" = case timeType of
    Time.MODIFICATION -> MODIFICATIONTIME
    Time.ACCESS -> ACCESSTIME
    Time.CHANGE -> CHANGETIME
  | otherwise = sortTypeFrom' sort
  where
    sort = Option.sort opt
    timeType = Option.time opt

sortTypeFrom' :: String -> SortType
sortTypeFrom' s = case s of
  "none" -> NONE
  "name" -> NAME
  "size" -> SIZE
  "time" -> MODIFICATIONTIME
  "modification_time" -> MODIFICATIONTIME
  "access_time" -> ACCESSTIME
  "change_time" -> CHANGETIME
  "version" -> VERSION
  "extension" -> EXTENSION
  _ -> NAME

sortWith :: SortType -> [Node.NodeInfo] -> [Node.NodeInfo]
sortWith sType = case sType of
  NONE -> sortWithNone
  NAME -> sortWithName
  SIZE -> sortWithSize
  MODIFICATIONTIME -> sortWithModificationTime
  ACCESSTIME -> sortWithAccessTime
  CHANGETIME -> sortWithChangeTime
  VERSION -> sortWithVersion
  EXTENSION -> sortWithExtension

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
sortWithVersion = id

sortWithExtension :: [Node.NodeInfo] -> [Node.NodeInfo]
sortWithExtension = id
