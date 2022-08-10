module Haskellorls.Walk.Sort (sort) where

import qualified Algorithms.NaturalSort as NSort
import Data.Char (isPunctuation)
import Data.List (partition, sortOn)
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU as T
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Sort
import qualified Haskellorls.NodeInfo as Node

-- | Compare naturally.
newtype NaturalS a = NaturalS a
  deriving (Eq)

instance (Eq a, NSort.NaturalSort a) => Ord (NaturalS a) where
  compare (NaturalS x) (NaturalS y) = x `NSort.compare` y

-- | Compare using ICU.
newtype ICUS a = ICUS a
  deriving (Eq)

instance Ord (ICUS T.Text) where
  compare (ICUS x) (ICUS y) = T.collate (T.collator T.Current) x y

sort :: Config.Config -> [Node.NodeInfo] -> [Node.NodeInfo]
sort config =
  merge . separate . case Config.sort config of
    NONE -> id
    NAME -> sortByName
    SIZE -> sortBySize
    TIME -> sortByTime
    VERSION -> sortByVersion
    EXTENSION -> sortByExtension
  where
    separate =
      if Config.groupDirectoriesFirst config && Config.sort config /= NONE
        then partition $ Node.isDirectory . Node.nodeType . Node.toFileInfo
        else ([],)
    merge (dirs, files) = order dirs <> order files
    order = if Config.reverse config then reverse else id

-- FIXME: Probably this sort isn't compatible with GNU ls. First of all, GNU ls
-- compares names using @strcoll@, and then fallbacks to @strcmp@ if fails.
sortByName :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByName = sortOn $ \node -> let path = toText node in (ICUS $ normalize path, ICUS path)

sortBySize :: [Node.NodeInfo] -> [Node.NodeInfo]
sortBySize = sortOn $ Down . Node.fileSize

sortByTime :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByTime = sortOn $ Down . Node.fileTime

sortByVersion :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByVersion = sortOn $ NaturalS . toText

sortByExtension :: [Node.NodeInfo] -> [Node.NodeInfo]
sortByExtension = sortOn $ \node -> let path = toText node in (ICUS . normalize $ T.takeWhileEnd (/= '.') path, ICUS $ normalize path, ICUS path)

-- | Create a "Text" for name comparison from "NodeInfo".
toText :: Node.NodeInfo -> T.Text
toText = T.decodeUtf8 . Node.getNodePath

-- | Normalize a "Text" for name comparison.
normalize :: T.Text -> T.Text
normalize = T.filter (not . isPunctuation)
