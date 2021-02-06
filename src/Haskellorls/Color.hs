{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Color
  ( Config (..),
    config,
    ExtensionConfig (..),
    lookupFilenameEscSec,
    toWrappedText,
    wrap,
  )
where

import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Haskellorls.WrappedText as WT
import qualified System.Environment as Env

type FilenamePtnMap = Map.Map T.Text T.Text

data Config = Config
  { leftEscapeSequence :: T.Text,
    rightEscapeSequence :: T.Text,
    endEscapeSequence :: T.Text,
    resetEscapeSequence :: T.Text,
    normalEscapeSequence :: T.Text,
    fileEscaseSequence :: T.Text,
    directoryEscapeSequence :: T.Text,
    symlinkEscapeSequence :: T.Text,
    pipeEscapeSequence :: T.Text,
    socketEscapeSequence :: T.Text,
    blockDeviceEscapeSequence :: T.Text,
    charDeviceEscapeSequence :: T.Text,
    missingFileEscapeSequence :: T.Text,
    orphanedSymlinkEscapeSequence :: T.Text,
    executableEscapeSequence :: T.Text,
    doorEscapeSequence :: T.Text,
    setuidEscapeSequence :: T.Text,
    setguiEscapeSequence :: T.Text,
    stickyEscapeSequence :: T.Text,
    otherWritableEscapeSequence :: T.Text,
    stickyOtherWritableEscapeSequence :: T.Text,
    capabilityEscapeSequence :: T.Text,
    multiHardlinkEscapeSequence :: T.Text,
    clearLineEscapeSequence :: T.Text,
    fileColorIndicator :: FilenamePtnMap,
    extensionColorConfig :: ExtensionConfig
  }

data ExtensionConfig = ExtensionConfig
  { userReadPermBitEscapeSequence :: T.Text,
    userWritePermBitEscapeSequence :: T.Text,
    userExecPermBitFileEscapeSequence :: T.Text,
    userExecPermBitOtherEscapeSequence :: T.Text,
    groupReadPermBitEscapeSequence :: T.Text,
    groupWritePermBitEscapeSequence :: T.Text,
    groupExecPermBitEscapeSequence :: T.Text,
    otherReadPermBitEscapeSequence :: T.Text,
    otherWritePermBitEscapeSequence :: T.Text,
    otherExecPermBitEscapeSequence :: T.Text,
    sPermBitFileEscapeSequence :: T.Text,
    sPermBitOtherEscapeSequence :: T.Text,
    ownerYourselfEscapeSequence :: T.Text,
    ownerNotYourselfEscapeSequence :: T.Text,
    groupYouBelongsToEscapeSequence :: T.Text,
    groupYouNotBelongsToEscapeSequence :: T.Text,
    fileSizeNumberEscapeSequence :: T.Text,
    fileSizeNumberBypeEscapeSequence :: T.Text,
    fileSizeNumberKiloEscapeSequence :: T.Text,
    fileSizeNumberMegaEscapeSequence :: T.Text,
    fileSizeNumberGigaEscapeSequence :: T.Text,
    fileSizeNumberTeraEscapeSequence :: T.Text,
    fileSizeNumberPetaEscapeSequence :: T.Text,
    fileSizeNumberExaEscapeSequence :: T.Text,
    fileSizeNumberZettaEscapeSequence :: T.Text,
    fileSizeNumberYottaEscapeSequence :: T.Text,
    fileSizeUnitBypeEscapeSequence :: T.Text,
    fileSizeUnitKiloEscapeSequence :: T.Text,
    fileSizeUnitMegaEscapeSequence :: T.Text,
    fileSizeUnitGigaEscapeSequence :: T.Text,
    fileSizeUnitTeraEscapeSequence :: T.Text,
    fileSizeUnitPetaEscapeSequence :: T.Text,
    fileSizeUnitExaEscapeSequence :: T.Text,
    fileSizeUnitZettaEscapeSequence :: T.Text,
    fileSizeUnitYottaEscapeSequence :: T.Text,
    dateEscapeSequence :: T.Text,
    fileLinkEscapeSequence :: T.Text,
    fileInodeEscapeSequence :: T.Text
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
    { userReadPermBitEscapeSequence = "1;32",
      userWritePermBitEscapeSequence = "1;31",
      userExecPermBitFileEscapeSequence = "1;33",
      userExecPermBitOtherEscapeSequence = "1;93",
      groupReadPermBitEscapeSequence = "32",
      groupWritePermBitEscapeSequence = "31",
      groupExecPermBitEscapeSequence = "33",
      otherReadPermBitEscapeSequence = "32",
      otherWritePermBitEscapeSequence = "31",
      otherExecPermBitEscapeSequence = "33",
      sPermBitFileEscapeSequence = "96",
      sPermBitOtherEscapeSequence = "96",
      ownerYourselfEscapeSequence = "35",
      ownerNotYourselfEscapeSequence = "",
      groupYouBelongsToEscapeSequence = "35",
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
      dateEscapeSequence = "34",
      fileLinkEscapeSequence = "36",
      fileInodeEscapeSequence = "36"
    }

config :: IO Config
config = configFrom . split <$> getLSCOLORS <> getEXACOLORS
  where
    split = T.split (== ':')

configFrom :: [T.Text] -> Config
configFrom lsColors =
  Config
    { leftEscapeSequence = Map.findWithDefault (leftEscapeSequence def) "lc" parametors,
      rightEscapeSequence = Map.findWithDefault (rightEscapeSequence def) "rc" parametors,
      endEscapeSequence = Map.findWithDefault (endEscapeSequence def) "ec" parametors,
      resetEscapeSequence = Map.findWithDefault (resetEscapeSequence def) "rs" parametors,
      normalEscapeSequence = Map.findWithDefault (normalEscapeSequence def) "no" parametors,
      fileEscaseSequence = Map.findWithDefault (fileEscaseSequence def) "fi" parametors,
      directoryEscapeSequence = Map.findWithDefault (directoryEscapeSequence def) "di" parametors,
      symlinkEscapeSequence = Map.findWithDefault (symlinkEscapeSequence def) "ln" parametors,
      pipeEscapeSequence = Map.findWithDefault (pipeEscapeSequence def) "pi" parametors,
      socketEscapeSequence = Map.findWithDefault (socketEscapeSequence def) "so" parametors,
      blockDeviceEscapeSequence = Map.findWithDefault (blockDeviceEscapeSequence def) "bd" parametors,
      charDeviceEscapeSequence = Map.findWithDefault (charDeviceEscapeSequence def) "cd" parametors,
      missingFileEscapeSequence = Map.findWithDefault (missingFileEscapeSequence def) "mi" parametors,
      orphanedSymlinkEscapeSequence = Map.findWithDefault (orphanedSymlinkEscapeSequence def) "or" parametors,
      executableEscapeSequence = Map.findWithDefault (executableEscapeSequence def) "ex" parametors,
      doorEscapeSequence = Map.findWithDefault (executableEscapeSequence def) "do" parametors,
      setuidEscapeSequence = Map.findWithDefault (setuidEscapeSequence def) "su" parametors,
      setguiEscapeSequence = Map.findWithDefault (setguiEscapeSequence def) "sg" parametors,
      stickyEscapeSequence = Map.findWithDefault (stickyEscapeSequence def) "st" parametors,
      otherWritableEscapeSequence = Map.findWithDefault (otherWritableEscapeSequence def) "ow" parametors,
      stickyOtherWritableEscapeSequence = Map.findWithDefault (stickyOtherWritableEscapeSequence def) "tw" parametors,
      capabilityEscapeSequence = Map.findWithDefault (capabilityEscapeSequence def) "ca" parametors,
      multiHardlinkEscapeSequence = Map.findWithDefault (multiHardlinkEscapeSequence def) "mh" parametors,
      clearLineEscapeSequence = Map.findWithDefault (clearLineEscapeSequence def) "cl" parametors,
      fileColorIndicator = indicator,
      extensionColorConfig = extensionConfigFrom lsColors
    }
  where
    def = defaultConfig
    indicator = colorIndicatorsFrom lsColors
    parametors = parametorsFrom lsColors

extensionConfigFrom :: [T.Text] -> ExtensionConfig
extensionConfigFrom lsColors =
  ExtensionConfig
    { userReadPermBitEscapeSequence = Map.findWithDefault (userReadPermBitEscapeSequence def) "ur" parametors,
      userWritePermBitEscapeSequence = Map.findWithDefault (userWritePermBitEscapeSequence def) "uw" parametors,
      userExecPermBitFileEscapeSequence = Map.findWithDefault (userExecPermBitFileEscapeSequence def) "ux" parametors,
      userExecPermBitOtherEscapeSequence = Map.findWithDefault (userExecPermBitOtherEscapeSequence def) "ue" parametors,
      groupReadPermBitEscapeSequence = Map.findWithDefault (groupReadPermBitEscapeSequence def) "gr" parametors,
      groupWritePermBitEscapeSequence = Map.findWithDefault (groupWritePermBitEscapeSequence def) "gw" parametors,
      groupExecPermBitEscapeSequence = Map.findWithDefault (groupExecPermBitEscapeSequence def) "gx" parametors,
      otherReadPermBitEscapeSequence = Map.findWithDefault (otherReadPermBitEscapeSequence def) "tr" parametors,
      otherWritePermBitEscapeSequence = Map.findWithDefault (otherWritePermBitEscapeSequence def) "tw'" parametors, -- WIP: `tw` is conflict between GNU LS and EXA
      otherExecPermBitEscapeSequence = Map.findWithDefault (otherExecPermBitEscapeSequence def) "tx" parametors,
      sPermBitFileEscapeSequence = Map.findWithDefault (sPermBitFileEscapeSequence def) "su" parametors,
      sPermBitOtherEscapeSequence = Map.findWithDefault (sPermBitOtherEscapeSequence def) "sf" parametors,
      ownerYourselfEscapeSequence = Map.findWithDefault (ownerYourselfEscapeSequence def) "uu" parametors,
      ownerNotYourselfEscapeSequence = Map.findWithDefault (ownerNotYourselfEscapeSequence def) "un" parametors,
      groupYouBelongsToEscapeSequence = Map.findWithDefault (groupYouBelongsToEscapeSequence def) "gu" parametors,
      groupYouNotBelongsToEscapeSequence = Map.findWithDefault (groupYouNotBelongsToEscapeSequence def) "gn" parametors,
      fileSizeNumberEscapeSequence = Map.findWithDefault (fileSizeNumberEscapeSequence def) "sn" parametors,
      fileSizeNumberBypeEscapeSequence = Map.findWithDefault (fileSizeNumberBypeEscapeSequence def) "nb" parametors,
      fileSizeNumberKiloEscapeSequence = Map.findWithDefault (fileSizeNumberKiloEscapeSequence def) "nk" parametors,
      fileSizeNumberMegaEscapeSequence = Map.findWithDefault (fileSizeNumberMegaEscapeSequence def) "nm" parametors,
      fileSizeNumberGigaEscapeSequence = Map.findWithDefault (fileSizeNumberGigaEscapeSequence def) "ng" parametors,
      fileSizeNumberTeraEscapeSequence = Map.findWithDefault (fileSizeNumberTeraEscapeSequence def) "nt" parametors,
      fileSizeNumberPetaEscapeSequence = Map.findWithDefault (fileSizeNumberPetaEscapeSequence def) "np" parametors,
      fileSizeNumberExaEscapeSequence = Map.findWithDefault (fileSizeNumberExaEscapeSequence def) "ne" parametors,
      fileSizeNumberZettaEscapeSequence = Map.findWithDefault (fileSizeNumberZettaEscapeSequence def) "nz" parametors,
      fileSizeNumberYottaEscapeSequence = Map.findWithDefault (fileSizeNumberYottaEscapeSequence def) "ny" parametors,
      fileSizeUnitBypeEscapeSequence = Map.findWithDefault (fileSizeUnitBypeEscapeSequence def) "ub" parametors,
      fileSizeUnitKiloEscapeSequence = Map.findWithDefault (fileSizeUnitKiloEscapeSequence def) "uk" parametors,
      fileSizeUnitMegaEscapeSequence = Map.findWithDefault (fileSizeUnitMegaEscapeSequence def) "um" parametors,
      fileSizeUnitGigaEscapeSequence = Map.findWithDefault (fileSizeUnitGigaEscapeSequence def) "ug" parametors,
      fileSizeUnitTeraEscapeSequence = Map.findWithDefault (fileSizeUnitTeraEscapeSequence def) "ut" parametors,
      fileSizeUnitPetaEscapeSequence = Map.findWithDefault (fileSizeUnitPetaEscapeSequence def) "up" parametors,
      fileSizeUnitExaEscapeSequence = Map.findWithDefault (fileSizeUnitExaEscapeSequence def) "ue" parametors,
      fileSizeUnitZettaEscapeSequence = Map.findWithDefault (fileSizeUnitZettaEscapeSequence def) "uz" parametors,
      fileSizeUnitYottaEscapeSequence = Map.findWithDefault (fileSizeUnitYottaEscapeSequence def) "uy" parametors,
      dateEscapeSequence = Map.findWithDefault (dateEscapeSequence def) "da" parametors,
      fileLinkEscapeSequence = Map.findWithDefault (fileLinkEscapeSequence def) "lc'" parametors,
      fileInodeEscapeSequence = Map.findWithDefault (fileInodeEscapeSequence def) "in" parametors
    }
  where
    def = defaultExtensionConfig
    parametors = parametorsFrom lsColors

colorIndicatorsFrom :: [T.Text] -> FilenamePtnMap
colorIndicatorsFrom = Map.fromList . Maybe.mapMaybe f
  where
    f s = makePatternEscapePair s >>= filenamePtnEscSec

filenamePtnEscSec :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
filenamePtnEscSec (ptn, esc) = filenamePattern ptn >>= (\ext -> Just (ext, esc))

filenamePattern :: T.Text -> Maybe T.Text
filenamePattern t
  | isPrefixWild t = Just . T.toUpper $ T.drop 1 t
  | otherwise = Nothing

parametorsFrom :: [T.Text] -> FilenamePtnMap
parametorsFrom = Map.fromList . Maybe.mapMaybe f
  where
    f s = makePatternEscapePair s >>= paramatorPtnEscSec

paramatorPtnEscSec :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
paramatorPtnEscSec pair@(ptn, _)
  | isPrefixWild ptn = Nothing
  | otherwise = Just pair

isPrefixWild :: T.Text -> Bool
isPrefixWild = Maybe.maybe False (\p -> fst p == '*') . T.uncons

makePatternEscapePair :: T.Text -> Maybe (T.Text, T.Text)
makePatternEscapePair s = case pairs of
  [k, v] -> Just (k, v)
  _ -> Nothing
  where
    pairs = T.split (== '=') s

getLSCOLORS :: IO T.Text
getLSCOLORS = Maybe.maybe "" T.pack <$> Env.lookupEnv "LS_COLORS"

getEXACOLORS :: IO T.Text
getEXACOLORS = Maybe.maybe "" T.pack <$> Env.lookupEnv "EXA_COLORS"

-- | Lookup ascii escape sequence. At first, lookup with a query as it is. If
--    fails to lookup, change a query to the extension and re lookup.
lookupFilenameEscSec :: FilenamePtnMap -> T.Text -> T.Text
lookupFilenameEscSec ptnMap = Extra.headDef "" . Maybe.mapMaybe (`Map.lookup` ptnMap) . reverse . T.tails . T.toUpper

toWrappedText :: Config -> (Config -> T.Text) -> T.Text -> WT.WrappedText
toWrappedText cConfig getter t =
  WT.WrappedText
    { WT.wtPrefix = wrap cConfig esc,
      WT.wtWord = t,
      WT.wtSuffix = wrap cConfig reset
    }
  where
    esc = getter cConfig
    reset = resetEscapeSequence cConfig

wrap :: Config -> T.Text -> T.Text
wrap cConfig t = left <> t <> right
  where
    left = leftEscapeSequence cConfig
    right = rightEscapeSequence cConfig
