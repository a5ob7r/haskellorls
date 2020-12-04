module Haskellorls.Color
  ( config
  , colorizedNodeName
  ) where

import Data.Char (toUpper)
import qualified Data.Map.Strict as Map (Map, empty, lookup, fromList)
import Data.List (isPrefixOf)
import Data.List.Extra (tails)
import Data.List.Split (endBy, splitOn)
import qualified Data.Maybe as Maybe (mapMaybe, fromMaybe)
import System.Environment (lookupEnv)

import Haskellorls.Node

type FilenamePtnMap = Map.Map String String

data Config = Config
  { leftEscapeSequence :: String
  , rightEscapeSequence :: String
  , endEscapeSequence :: String
  , resetEscapeSequence :: String
  , normalEscapeSequence :: String
  , fileEscaseSequence :: String
  , directoryEscapeSequence :: String
  , symlinkEscapeSequence :: String
  , pipeEscapeSequence :: String
  , socketEscapeSequence :: String
  , blockDeviceEscapeSequence :: String
  , charDeviceEscapeSequence :: String
  , missingFileEscapeSequence :: String
  , orphanedSymlink :: String
  , executableEscapeSequence :: String
  , doorEscapeSequence :: String
  , setuidEscapeSequence :: String
  , setguiEscapeSequence :: String
  , stickyEscapeSequence :: String
  , otherWritableEscapeSequence :: String
  , stickyOtherWritableEscapeSequence :: String
  , capabilityEscapeSequence :: String
  , multiHardlinkEscapeSequence :: String
  , clearLineEscapeSequence :: String
  , fileColorIndicator :: FilenamePtnMap
  }

defaultConfig :: Config
defaultConfig = Config
  { leftEscapeSequence = "\^[["
  , rightEscapeSequence = "m"
  , endEscapeSequence = ""
  , resetEscapeSequence = "0"
  , normalEscapeSequence = ""
  , fileEscaseSequence = ""
  , directoryEscapeSequence = "01;34"
  , symlinkEscapeSequence = "01;36"
  , pipeEscapeSequence = "33"
  , socketEscapeSequence = "01;35"
  , blockDeviceEscapeSequence = "01;33"
  , charDeviceEscapeSequence = "01;33"
  , missingFileEscapeSequence = ""
  , orphanedSymlink = ""
  , executableEscapeSequence = "01;32"
  , doorEscapeSequence = "01;35"
  , setuidEscapeSequence = "37;41"
  , setguiEscapeSequence = "30;43"
  , stickyEscapeSequence = "37;44"
  , otherWritableEscapeSequence = "34;42"
  , stickyOtherWritableEscapeSequence = "30;42"
  , capabilityEscapeSequence = "30;41"
  , multiHardlinkEscapeSequence = ""
  , clearLineEscapeSequence = "\^[[K"
  , fileColorIndicator = Map.empty
  }

config :: IO Config
config = configFrom <$> getLSCOLORS

configFrom :: String -> Config
configFrom lsColors = Config
  { leftEscapeSequence = "\^[["
  , rightEscapeSequence = "m"
  , endEscapeSequence = ""
  , resetEscapeSequence = "0"
  , normalEscapeSequence = ""
  , fileEscaseSequence = ""
  , directoryEscapeSequence = Maybe.fromMaybe (directoryEscapeSequence def) $ "di" `Map.lookup` parametors
  , symlinkEscapeSequence = symlinkEscapeSequence def
  , pipeEscapeSequence = Maybe.fromMaybe (pipeEscapeSequence def) $ "pi" `Map.lookup` parametors
  , socketEscapeSequence = Maybe.fromMaybe (socketEscapeSequence def) $ "so" `Map.lookup` parametors
  , blockDeviceEscapeSequence = Maybe.fromMaybe (blockDeviceEscapeSequence def) $ "bd" `Map.lookup` parametors
  , charDeviceEscapeSequence = Maybe.fromMaybe (charDeviceEscapeSequence def) $ "cd" `Map.lookup` parametors
  , missingFileEscapeSequence = ""
  , orphanedSymlink = ""
  , executableEscapeSequence = Maybe.fromMaybe (executableEscapeSequence def) $ "ex" `Map.lookup` parametors
  , doorEscapeSequence = Maybe.fromMaybe (executableEscapeSequence def) $ "do" `Map.lookup` parametors
  , setuidEscapeSequence = Maybe.fromMaybe (setuidEscapeSequence def) $ "su" `Map.lookup` parametors
  , setguiEscapeSequence = Maybe.fromMaybe (setguiEscapeSequence def) $ "sg" `Map.lookup` parametors
  , stickyEscapeSequence = Maybe.fromMaybe (stickyEscapeSequence def) $ "st" `Map.lookup` parametors
  , otherWritableEscapeSequence = "34;42"
  , stickyOtherWritableEscapeSequence = Maybe.fromMaybe (stickyOtherWritableEscapeSequence def) $ "tw" `Map.lookup` parametors
  , capabilityEscapeSequence = "30;41"
  , multiHardlinkEscapeSequence = ""
  , clearLineEscapeSequence = "\^[[K"
  , fileColorIndicator = indicator
  }
    where
      def = defaultConfig
      indicator = colorIndicatorsFrom lsColors
      parametors = parametorsFrom lsColors

colorizedNodeName :: Config -> Node -> String
colorizedNodeName conf nd = start ++ name ++ end
    where
      left = leftEscapeSequence conf
      right = rightEscapeSequence conf
      end = left ++ right
      start = left ++ escSec ++ right
      name = nodeName nd
      escSec = lookupEscSec conf nd

lookupEscSec :: Config -> Node -> String
lookupEscSec conf nd = case nodeType nd of
  Directory -> directoryEscapeSequence conf
  SymbolicLink -> symlinkEscapeSequence conf
  NamedPipe -> pipeEscapeSequence conf
  Socket -> socketEscapeSequence conf
  BlockDevise -> blockDeviceEscapeSequence conf
  CharDevise -> charDeviceEscapeSequence conf
  DoorsDevise -> doorEscapeSequence conf
  Setuid -> setuidEscapeSequence conf
  Setgid -> setguiEscapeSequence conf
  Sticky -> stickyEscapeSequence conf
  StickyOtherWritable -> stickyOtherWritableEscapeSequence conf
  OtherWritable -> otherWritableEscapeSequence conf
  Executable -> executableEscapeSequence conf
  File -> lookupFilenameEscSec (fileColorIndicator conf) $ nodeName nd

{-| Lookup ascii escape sequence. At first, lookup with a query as it is. If
    fails to lookup, change a query to the extension and re lookup.
-}
lookupFilenameEscSec :: FilenamePtnMap -> String -> String
lookupFilenameEscSec ptnMap = f . Maybe.mapMaybe (`Map.lookup` ptnMap) . reverse . tails . toUppers
  where f [] = ""
        f xs = head xs

colorIndicatorsFrom :: String -> FilenamePtnMap
colorIndicatorsFrom = Map.fromList . Maybe.mapMaybe f . endBy ":"
    where f s = makePatternEscapePair s >>= filenamePtnEscSec

filenamePtnEscSec :: (String, String) -> Maybe (String, String)
filenamePtnEscSec (ptn, esc) = filenamePattern ptn >>= (\ext -> Just (ext, esc))

filenamePattern :: String -> Maybe String
filenamePattern str = if "*" `isPrefixOf` str
                         then Just . toUppers . drop 1 $ str
                         else Nothing

parametorsFrom :: String -> FilenamePtnMap
parametorsFrom = Map.fromList . Maybe.mapMaybe f . endBy ":"
  where f s = makePatternEscapePair s >>= paramatorPtnEscSec

paramatorPtnEscSec :: (String, String) -> Maybe (String, String)
paramatorPtnEscSec (ptn, esc) = if "*" `isPrefixOf` ptn
                                   then Nothing
                                   else Just (ptn, esc)

makePatternEscapePair :: String -> Maybe (String, String)
makePatternEscapePair s = if length pairs == 2
                             then Just (head pairs, last pairs)
                             else Nothing
  where pairs = splitOn "=" s

getLSCOLORS :: IO String
getLSCOLORS = Maybe.fromMaybe "" <$> lookupEnv "LS_COLORS"

toUppers :: String -> String
toUppers = map toUpper
