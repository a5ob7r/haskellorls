module Haskellorls.Walk.Sort (sort) where

import Algorithms.NaturalSort qualified as NSort
import Data.Char (isPunctuation)
import Data.List (partition, sortOn)
import Data.Ord
import Data.Text qualified as T
import Haskellorls.Class (termLength)
import Haskellorls.Config qualified as Config
import Haskellorls.Config.Sort
import Haskellorls.NodeInfo qualified as Node
import Haskellorls.System.OsPath.Posix.Extra (decode)

sort :: Config.Config -> [Node.NodeInfo] -> [Node.NodeInfo]
sort config =
  merge . separate . case Config.sort config of
    NONE -> id
    NAME -> sortByName
    SIZE -> sortBySize
    TIME -> sortByTime
    VERSION -> sortByVersion
    EXTENSION -> sortByExtension
    WIDTH -> sortByWidth
  where
    separate =
      if Config.groupDirectoriesFirst config && Config.sort config /= NONE
        then partition $ maybe False Node.isDirectory . Node.nodeType . Node.dereference
        else ([],)
    merge (dirs, files) = order dirs <> order files
    order = if Config.reverse config then reverse else id

-- FIXME: Probably this sort isn't compatible with GNU ls. First of all, GNU ls
-- compares names using @strcoll@, and then fallbacks to @strcmp@ if fails.
sortByName :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByName = sortOn $ \node -> let path = toText node in (normalize path, path)

sortBySize :: [Node.NodeInfo] -> [Node.NodeInfo]
sortBySize = sortOn $ Down . Node.fileSize

sortByTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByTime = sortOn $ Down . Node.fileTime

sortByVersion :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByVersion = sortOn $ NSort.sortKey . toText

sortByExtension :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByExtension = sortOn $ \node -> let path = toText node in (normalize $ T.takeWhileEnd (/= '.') path, normalize path, path)

sortByWidth :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByWidth = sortOn $ \node -> let path = toText node in (termLength path, path)

-- | Create a "Text" for name comparison from "NodeInfo".
toText :: Node.NodeInfo -> T.Text
toText = T.pack . decode . Node.getNodePath

-- | Normalize a "Text" for name comparison.
normalize :: T.Text -> T.Text
normalize = T.toUpper . T.filter (not . isPunctuation)
