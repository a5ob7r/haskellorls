module Haskellorls.Color
  ( Config (..),
    config,
    ExtensionConfig (..),
    applyEscapeSequence,
    lookupFilenameEscSec,
  )
where

import Data.List.Extra (tails)
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Data.List.Split (endBy, splitOn)
import qualified Data.Map.Strict as Map (Map, empty, fromList, lookup)
import qualified Data.Maybe as Maybe (fromMaybe, mapMaybe)
import System.Environment (lookupEnv)

type FilenamePtnMap = Map.Map String String

data Config = Config
  { leftEscapeSequence :: String,
    rightEscapeSequence :: String,
    endEscapeSequence :: String,
    resetEscapeSequence :: String,
    normalEscapeSequence :: String,
    fileEscaseSequence :: String,
    directoryEscapeSequence :: String,
    symlinkEscapeSequence :: String,
    pipeEscapeSequence :: String,
    socketEscapeSequence :: String,
    blockDeviceEscapeSequence :: String,
    charDeviceEscapeSequence :: String,
    missingFileEscapeSequence :: String,
    orphanedSymlinkEscapeSequence :: String,
    executableEscapeSequence :: String,
    doorEscapeSequence :: String,
    setuidEscapeSequence :: String,
    setguiEscapeSequence :: String,
    stickyEscapeSequence :: String,
    otherWritableEscapeSequence :: String,
    stickyOtherWritableEscapeSequence :: String,
    capabilityEscapeSequence :: String,
    multiHardlinkEscapeSequence :: String,
    clearLineEscapeSequence :: String,
    fileColorIndicator :: FilenamePtnMap,
    extensionColorConfig :: ExtensionConfig
  }

data ExtensionConfig = ExtensionConfig
  { userReadPermBitEscapeSequence :: String,
    userWritePermBitEscapeSequence :: String,
    userExecPermBitFileEscapeSequence :: String,
    userExecPermBitOtherEscapeSequence :: String,
    groupReadPermBitEscapeSequence :: String,
    groupWritePermBitEscapeSequence :: String,
    groupExecPermBitEscapeSequence :: String,
    otherReadPermBitEscapeSequence :: String,
    otherWritePermBitEscapeSequence :: String,
    otherExecPermBitEscapeSequence :: String,
    sPermBitFileEscapeSequence :: String,
    sPermBitOtherEscapeSequence :: String,
    ownerYourselfEscapeSequence :: String,
    ownerNotYourselfEscapeSequence :: String,
    groupYouBelongsToEscapeSequence :: String,
    groupYouNotBelongsToEscapeSequence :: String,
    fileSizeNumberEscapeSequence :: String,
    fileSizeNumberBypeEscapeSequence :: String,
    fileSizeNumberKiloEscapeSequence :: String,
    fileSizeNumberMegaEscapeSequence :: String,
    fileSizeNumberGigaEscapeSequence :: String,
    fileSizeNumberTeraEscapeSequence :: String,
    fileSizeNumberPetaEscapeSequence :: String,
    fileSizeNumberExaEscapeSequence :: String,
    fileSizeNumberZettaEscapeSequence :: String,
    fileSizeNumberYottaEscapeSequence :: String,
    fileSizeUnitBypeEscapeSequence :: String,
    fileSizeUnitKiloEscapeSequence :: String,
    fileSizeUnitMegaEscapeSequence :: String,
    fileSizeUnitGigaEscapeSequence :: String,
    fileSizeUnitTeraEscapeSequence :: String,
    fileSizeUnitPetaEscapeSequence :: String,
    fileSizeUnitExaEscapeSequence :: String,
    fileSizeUnitZettaEscapeSequence :: String,
    fileSizeUnitYottaEscapeSequence :: String,
    dateEscapeSequence :: String
  }

defaultConfig :: Config
defaultConfig =
  Config
    { leftEscapeSequence = "\^[[",
      rightEscapeSequence = "m",
      endEscapeSequence = "",
      resetEscapeSequence = "0",
      normalEscapeSequence = "",
      fileEscaseSequence = "",
      directoryEscapeSequence = "01;34",
      symlinkEscapeSequence = "01;36",
      pipeEscapeSequence = "33",
      socketEscapeSequence = "01;35",
      blockDeviceEscapeSequence = "01;33",
      charDeviceEscapeSequence = "01;33",
      missingFileEscapeSequence = "",
      orphanedSymlinkEscapeSequence = "",
      executableEscapeSequence = "01;32",
      doorEscapeSequence = "01;35",
      setuidEscapeSequence = "37;41",
      setguiEscapeSequence = "30;43",
      stickyEscapeSequence = "37;44",
      otherWritableEscapeSequence = "34;42",
      stickyOtherWritableEscapeSequence = "30;42",
      capabilityEscapeSequence = "30;41",
      multiHardlinkEscapeSequence = "",
      clearLineEscapeSequence = "\^[[K",
      fileColorIndicator = Map.empty,
      extensionColorConfig = defaultExtensionConfig
    }

defaultExtensionConfig :: ExtensionConfig
defaultExtensionConfig =
  ExtensionConfig
    { userReadPermBitEscapeSequence = "1;33",
      userWritePermBitEscapeSequence = "1;31",
      userExecPermBitFileEscapeSequence = "1;32",
      userExecPermBitOtherEscapeSequence = "1;92",
      groupReadPermBitEscapeSequence = "1;33",
      groupWritePermBitEscapeSequence = "1;31",
      groupExecPermBitEscapeSequence = "1;32",
      otherReadPermBitEscapeSequence = "1;33",
      otherWritePermBitEscapeSequence = "1;31",
      otherExecPermBitEscapeSequence = "1;32",
      sPermBitFileEscapeSequence = "1;96",
      sPermBitOtherEscapeSequence = "1;96",
      ownerYourselfEscapeSequence = "1;33",
      ownerNotYourselfEscapeSequence = "",
      groupYouBelongsToEscapeSequence = "1;33",
      groupYouNotBelongsToEscapeSequence = "",
      fileSizeNumberEscapeSequence = "1;32",
      fileSizeNumberBypeEscapeSequence = "1;32",
      fileSizeNumberKiloEscapeSequence = "1;32",
      fileSizeNumberMegaEscapeSequence = "1;32",
      fileSizeNumberGigaEscapeSequence = "1;32",
      fileSizeNumberTeraEscapeSequence = "1;32",
      fileSizeNumberPetaEscapeSequence = "1;32",
      fileSizeNumberExaEscapeSequence = "1;32",
      fileSizeNumberZettaEscapeSequence = "1;32",
      fileSizeNumberYottaEscapeSequence = "1;32",
      fileSizeUnitBypeEscapeSequence = "32",
      fileSizeUnitKiloEscapeSequence = "32",
      fileSizeUnitMegaEscapeSequence = "32",
      fileSizeUnitGigaEscapeSequence = "32",
      fileSizeUnitTeraEscapeSequence = "32",
      fileSizeUnitPetaEscapeSequence = "32",
      fileSizeUnitExaEscapeSequence = "32",
      fileSizeUnitZettaEscapeSequence = "32",
      fileSizeUnitYottaEscapeSequence = "32",
      dateEscapeSequence = "34"
    }

config :: IO Config
config = configFrom <$> getLSCOLORS

configFrom :: String -> Config
configFrom lsColors =
  Config
    { leftEscapeSequence = "\^[[",
      rightEscapeSequence = "m",
      endEscapeSequence = "",
      resetEscapeSequence = "0",
      normalEscapeSequence = "",
      fileEscaseSequence = "",
      directoryEscapeSequence = Maybe.fromMaybe (directoryEscapeSequence def) $ "di" `Map.lookup` parametors,
      symlinkEscapeSequence = Maybe.fromMaybe (symlinkEscapeSequence def) $ "ln" `Map.lookup` parametors,
      pipeEscapeSequence = Maybe.fromMaybe (pipeEscapeSequence def) $ "pi" `Map.lookup` parametors,
      socketEscapeSequence = Maybe.fromMaybe (socketEscapeSequence def) $ "so" `Map.lookup` parametors,
      blockDeviceEscapeSequence = Maybe.fromMaybe (blockDeviceEscapeSequence def) $ "bd" `Map.lookup` parametors,
      charDeviceEscapeSequence = Maybe.fromMaybe (charDeviceEscapeSequence def) $ "cd" `Map.lookup` parametors,
      missingFileEscapeSequence = "",
      orphanedSymlinkEscapeSequence = Maybe.fromMaybe (orphanedSymlinkEscapeSequence def) $ "or" `Map.lookup` parametors,
      executableEscapeSequence = Maybe.fromMaybe (executableEscapeSequence def) $ "ex" `Map.lookup` parametors,
      doorEscapeSequence = Maybe.fromMaybe (executableEscapeSequence def) $ "do" `Map.lookup` parametors,
      setuidEscapeSequence = Maybe.fromMaybe (setuidEscapeSequence def) $ "su" `Map.lookup` parametors,
      setguiEscapeSequence = Maybe.fromMaybe (setguiEscapeSequence def) $ "sg" `Map.lookup` parametors,
      stickyEscapeSequence = Maybe.fromMaybe (stickyEscapeSequence def) $ "st" `Map.lookup` parametors,
      otherWritableEscapeSequence = "34;42",
      stickyOtherWritableEscapeSequence = Maybe.fromMaybe (stickyOtherWritableEscapeSequence def) $ "tw" `Map.lookup` parametors,
      capabilityEscapeSequence = "30;41",
      multiHardlinkEscapeSequence = "",
      clearLineEscapeSequence = "\^[[K",
      fileColorIndicator = indicator,
      extensionColorConfig = extensionConfigFrom ""
    }
  where
    def = defaultConfig
    indicator = colorIndicatorsFrom lsColors
    parametors = parametorsFrom lsColors

extensionConfigFrom :: String -> ExtensionConfig
extensionConfigFrom lsColors =
  ExtensionConfig
    { userReadPermBitEscapeSequence = Maybe.fromMaybe (userReadPermBitEscapeSequence def) $ "ur" `Map.lookup` parametors,
      userWritePermBitEscapeSequence = Maybe.fromMaybe (userWritePermBitEscapeSequence def) $ "uw" `Map.lookup` parametors,
      userExecPermBitFileEscapeSequence = Maybe.fromMaybe (userExecPermBitFileEscapeSequence def) $ "ux" `Map.lookup` parametors,
      userExecPermBitOtherEscapeSequence = Maybe.fromMaybe (userExecPermBitOtherEscapeSequence def) $ "ue" `Map.lookup` parametors,
      groupReadPermBitEscapeSequence = Maybe.fromMaybe (groupReadPermBitEscapeSequence def) $ "gr" `Map.lookup` parametors,
      groupWritePermBitEscapeSequence = Maybe.fromMaybe (groupWritePermBitEscapeSequence def) $ "gw" `Map.lookup` parametors,
      groupExecPermBitEscapeSequence = Maybe.fromMaybe (groupExecPermBitEscapeSequence def) $ "gx" `Map.lookup` parametors,
      otherReadPermBitEscapeSequence = Maybe.fromMaybe (otherReadPermBitEscapeSequence def) $ "tr" `Map.lookup` parametors,
      otherWritePermBitEscapeSequence = Maybe.fromMaybe (otherWritePermBitEscapeSequence def) $ "tw" `Map.lookup` parametors,
      otherExecPermBitEscapeSequence = Maybe.fromMaybe (otherExecPermBitEscapeSequence def) $ "tx" `Map.lookup` parametors,
      sPermBitFileEscapeSequence = Maybe.fromMaybe (sPermBitFileEscapeSequence def) $ "su" `Map.lookup` parametors,
      sPermBitOtherEscapeSequence = Maybe.fromMaybe (sPermBitOtherEscapeSequence def) $ "sf" `Map.lookup` parametors,
      ownerYourselfEscapeSequence = Maybe.fromMaybe (ownerYourselfEscapeSequence def) $ "uu" `Map.lookup` parametors,
      ownerNotYourselfEscapeSequence = Maybe.fromMaybe (ownerNotYourselfEscapeSequence def) $ "un" `Map.lookup` parametors,
      groupYouBelongsToEscapeSequence = Maybe.fromMaybe (groupYouBelongsToEscapeSequence def) $ "gu" `Map.lookup` parametors,
      groupYouNotBelongsToEscapeSequence = Maybe.fromMaybe (groupYouNotBelongsToEscapeSequence def) $ "gn" `Map.lookup` parametors,
      fileSizeNumberEscapeSequence = Maybe.fromMaybe (fileSizeNumberEscapeSequence def) $ "sn" `Map.lookup` parametors,
      fileSizeNumberBypeEscapeSequence = Maybe.fromMaybe (fileSizeNumberBypeEscapeSequence def) $ "nb" `Map.lookup` parametors,
      fileSizeNumberKiloEscapeSequence = Maybe.fromMaybe (fileSizeNumberKiloEscapeSequence def) $ "nk" `Map.lookup` parametors,
      fileSizeNumberMegaEscapeSequence = Maybe.fromMaybe (fileSizeNumberMegaEscapeSequence def) $ "nm" `Map.lookup` parametors,
      fileSizeNumberGigaEscapeSequence = Maybe.fromMaybe (fileSizeNumberGigaEscapeSequence def) $ "ng" `Map.lookup` parametors,
      fileSizeNumberTeraEscapeSequence = Maybe.fromMaybe (fileSizeNumberTeraEscapeSequence def) $ "nt" `Map.lookup` parametors,
      fileSizeNumberPetaEscapeSequence = Maybe.fromMaybe (fileSizeNumberPetaEscapeSequence def) $ "np" `Map.lookup` parametors,
      fileSizeNumberExaEscapeSequence = Maybe.fromMaybe (fileSizeNumberExaEscapeSequence def) $ "ne" `Map.lookup` parametors,
      fileSizeNumberZettaEscapeSequence = Maybe.fromMaybe (fileSizeNumberZettaEscapeSequence def) $ "nz" `Map.lookup` parametors,
      fileSizeNumberYottaEscapeSequence = Maybe.fromMaybe (fileSizeNumberYottaEscapeSequence def) $ "ny" `Map.lookup` parametors,
      fileSizeUnitBypeEscapeSequence = Maybe.fromMaybe (fileSizeUnitBypeEscapeSequence def) $ "ub" `Map.lookup` parametors,
      fileSizeUnitKiloEscapeSequence = Maybe.fromMaybe (fileSizeUnitKiloEscapeSequence def) $ "uk" `Map.lookup` parametors,
      fileSizeUnitMegaEscapeSequence = Maybe.fromMaybe (fileSizeUnitMegaEscapeSequence def) $ "um" `Map.lookup` parametors,
      fileSizeUnitGigaEscapeSequence = Maybe.fromMaybe (fileSizeUnitGigaEscapeSequence def) $ "ug" `Map.lookup` parametors,
      fileSizeUnitTeraEscapeSequence = Maybe.fromMaybe (fileSizeUnitTeraEscapeSequence def) $ "ut" `Map.lookup` parametors,
      fileSizeUnitPetaEscapeSequence = Maybe.fromMaybe (fileSizeUnitPetaEscapeSequence def) $ "up" `Map.lookup` parametors,
      fileSizeUnitExaEscapeSequence = Maybe.fromMaybe (fileSizeUnitExaEscapeSequence def) $ "ue" `Map.lookup` parametors,
      fileSizeUnitZettaEscapeSequence = Maybe.fromMaybe (fileSizeUnitZettaEscapeSequence def) $ "uz" `Map.lookup` parametors,
      fileSizeUnitYottaEscapeSequence = Maybe.fromMaybe (fileSizeUnitYottaEscapeSequence def) $ "uy" `Map.lookup` parametors,
      dateEscapeSequence = Maybe.fromMaybe (dateEscapeSequence def) $ "da" `Map.lookup` parametors
    }
  where
    def = defaultExtensionConfig
    parametors = parametorsFrom lsColors

applyEscapeSequence :: Config -> String -> String
applyEscapeSequence conf escSeq = left ++ escSeq ++ right
  where
    left = leftEscapeSequence conf
    right = rightEscapeSequence conf

colorIndicatorsFrom :: String -> FilenamePtnMap
colorIndicatorsFrom = Map.fromList . Maybe.mapMaybe f . endBy ":"
  where
    f s = makePatternEscapePair s >>= filenamePtnEscSec

filenamePtnEscSec :: (String, String) -> Maybe (String, String)
filenamePtnEscSec (ptn, esc) = filenamePattern ptn >>= (\ext -> Just (ext, esc))

filenamePattern :: String -> Maybe String
filenamePattern str =
  if "*" `isPrefixOf` str
    then Just . toUppers . drop 1 $ str
    else Nothing

parametorsFrom :: String -> FilenamePtnMap
parametorsFrom = Map.fromList . Maybe.mapMaybe f . endBy ":"
  where
    f s = makePatternEscapePair s >>= paramatorPtnEscSec

paramatorPtnEscSec :: (String, String) -> Maybe (String, String)
paramatorPtnEscSec (ptn, esc) =
  if "*" `isPrefixOf` ptn
    then Nothing
    else Just (ptn, esc)

makePatternEscapePair :: String -> Maybe (String, String)
makePatternEscapePair s =
  if length pairs == 2
    then Just (head pairs, last pairs)
    else Nothing
  where
    pairs = splitOn "=" s

getLSCOLORS :: IO String
getLSCOLORS = Maybe.fromMaybe "" <$> lookupEnv "LS_COLORS"

-- | Lookup ascii escape sequence. At first, lookup with a query as it is. If
--    fails to lookup, change a query to the extension and re lookup.
lookupFilenameEscSec :: FilenamePtnMap -> String -> String
lookupFilenameEscSec ptnMap = f . Maybe.mapMaybe (`Map.lookup` ptnMap) . reverse . tails . toUppers
  where
    f [] = ""
    f xs = head xs

toUppers :: String -> String
toUppers = map toUpper
