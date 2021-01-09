module Haskellorls.Color
  ( Config(..)
  , config
  , ExtensionConfig (..)
  , colorizedNodeName
  , nodeName
  , applyEscapeSequence
  ) where

import Data.Char (toUpper)
import qualified Data.Map.Strict as Map (Map, empty, lookup, fromList)
import Data.List (isPrefixOf)
import Data.List.Extra (tails)
import Data.List.Split (endBy, splitOn)
import qualified Data.Maybe as Maybe (mapMaybe, fromMaybe)
import System.Environment (lookupEnv)
import qualified System.FilePath.Posix as Posix (takeFileName)
import qualified System.Posix.Files as Files
    ( FileStatus
    , isRegularFile
    , isDirectory
    , isSymbolicLink
    , isNamedPipe
    , isSocket
    , isBlockDevice
    , isCharacterDevice
    , ownerExecuteMode
    , groupExecuteMode
    , otherWriteMode
    , otherExecuteMode
    , intersectFileModes
    , unionFileModes
    , setGroupIDMode
    , setUserIDMode
    , fileMode
    )
import qualified System.Posix.Types as Types (FileMode)

import qualified Haskellorls.NodeInfo as Node

type FilenamePtnMap = Map.Map String String

data NodeType
  = Directory
  | SymbolicLink
  | NamedPipe
  | Socket
  | BlockDevise
  | CharDevise
  | DoorsDevise -- NOTE: Doors device is not implemented on Linux
  | Setuid
  | Setgid
  | Sticky
  | StickyOtherWritable
  | OtherWritable
  | Executable
  | File
  | Orphan
  deriving (Show)

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
  , orphanedSymlinkEscapeSequence :: String
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
  , extensionColorConfig :: ExtensionConfig
  }

data ExtensionConfig = ExtensionConfig
  { userReadPermBitEscapeSequence :: String
  , userWritePermBitEscapeSequence :: String
  , userExecPermBitFileEscapeSequence :: String
  , userExecPermBitOtherEscapeSequence :: String
  , groupReadPermBitEscapeSequence :: String
  , groupWritePermBitEscapeSequence :: String
  , groupExecPermBitEscapeSequence :: String
  , otherReadPermBitEscapeSequence :: String
  , otherWritePermBitEscapeSequence :: String
  , otherExecPermBitEscapeSequence :: String
  , sPermBitFileEscapeSequence :: String
  , sPermBitOtherEscapeSequence :: String
  , ownerYourselfEscapeSequence :: String
  , ownerNotYourselfEscapeSequence :: String
  , groupYouBelongsToEscapeSequence :: String
  , groupYouNotBelongsToEscapeSequence :: String
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
  , orphanedSymlinkEscapeSequence = ""
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
  , extensionColorConfig = defaultExtensionConfig
  }

defaultExtensionConfig :: ExtensionConfig
defaultExtensionConfig = ExtensionConfig
  { userReadPermBitEscapeSequence = "1;33"
  , userWritePermBitEscapeSequence = "1;31"
  , userExecPermBitFileEscapeSequence = "1;32"
  , userExecPermBitOtherEscapeSequence = "1;92"
  , groupReadPermBitEscapeSequence = "1;33"
  , groupWritePermBitEscapeSequence = "1;31"
  , groupExecPermBitEscapeSequence = "1;32"
  , otherReadPermBitEscapeSequence = "1;33"
  , otherWritePermBitEscapeSequence = "1;31"
  , otherExecPermBitEscapeSequence = "1;32"
  , sPermBitFileEscapeSequence = "1;96"
  , sPermBitOtherEscapeSequence = "1;96"
  , ownerYourselfEscapeSequence = "1;33"
  , ownerNotYourselfEscapeSequence = ""
  , groupYouBelongsToEscapeSequence = "1;33"
  , groupYouNotBelongsToEscapeSequence = ""
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
  , symlinkEscapeSequence = Maybe.fromMaybe (symlinkEscapeSequence def) $ "ln" `Map.lookup` parametors
  , pipeEscapeSequence = Maybe.fromMaybe (pipeEscapeSequence def) $ "pi" `Map.lookup` parametors
  , socketEscapeSequence = Maybe.fromMaybe (socketEscapeSequence def) $ "so" `Map.lookup` parametors
  , blockDeviceEscapeSequence = Maybe.fromMaybe (blockDeviceEscapeSequence def) $ "bd" `Map.lookup` parametors
  , charDeviceEscapeSequence = Maybe.fromMaybe (charDeviceEscapeSequence def) $ "cd" `Map.lookup` parametors
  , missingFileEscapeSequence = ""
  , orphanedSymlinkEscapeSequence = Maybe.fromMaybe (orphanedSymlinkEscapeSequence def) $ "or" `Map.lookup` parametors
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
  , extensionColorConfig = extensionConfigFrom ""
  }
    where
      def = defaultConfig
      indicator = colorIndicatorsFrom lsColors
      parametors = parametorsFrom lsColors

extensionConfigFrom :: String -> ExtensionConfig
extensionConfigFrom lsColors = ExtensionConfig
  { userReadPermBitEscapeSequence = Maybe.fromMaybe (userReadPermBitEscapeSequence def) $ "ur" `Map.lookup` parametors
  , userWritePermBitEscapeSequence = Maybe.fromMaybe (userWritePermBitEscapeSequence def) $ "uw" `Map.lookup` parametors
  , userExecPermBitFileEscapeSequence = Maybe.fromMaybe (userExecPermBitFileEscapeSequence def) $ "ux" `Map.lookup` parametors
  , userExecPermBitOtherEscapeSequence = Maybe.fromMaybe (userExecPermBitOtherEscapeSequence def) $ "ue" `Map.lookup` parametors
  , groupReadPermBitEscapeSequence = Maybe.fromMaybe (groupReadPermBitEscapeSequence def) $ "gr" `Map.lookup` parametors
  , groupWritePermBitEscapeSequence = Maybe.fromMaybe (groupWritePermBitEscapeSequence def) $ "gw" `Map.lookup` parametors
  , groupExecPermBitEscapeSequence = Maybe.fromMaybe (groupExecPermBitEscapeSequence def) $ "gx" `Map.lookup` parametors
  , otherReadPermBitEscapeSequence = Maybe.fromMaybe (otherReadPermBitEscapeSequence def) $ "tr" `Map.lookup` parametors
  , otherWritePermBitEscapeSequence = Maybe.fromMaybe (otherWritePermBitEscapeSequence def) $ "tw" `Map.lookup` parametors
  , otherExecPermBitEscapeSequence = Maybe.fromMaybe (otherExecPermBitEscapeSequence def) $ "tx" `Map.lookup` parametors
  , sPermBitFileEscapeSequence = Maybe.fromMaybe (sPermBitFileEscapeSequence def) $ "su" `Map.lookup` parametors
  , sPermBitOtherEscapeSequence = Maybe.fromMaybe (sPermBitOtherEscapeSequence def) $ "sf" `Map.lookup` parametors
  , ownerYourselfEscapeSequence = Maybe.fromMaybe (ownerYourselfEscapeSequence def) $ "uu" `Map.lookup` parametors
  , ownerNotYourselfEscapeSequence = Maybe.fromMaybe (ownerNotYourselfEscapeSequence def) $ "un" `Map.lookup` parametors
  , groupYouBelongsToEscapeSequence = Maybe.fromMaybe (groupYouBelongsToEscapeSequence def) $ "gu" `Map.lookup` parametors
  , groupYouNotBelongsToEscapeSequence = Maybe.fromMaybe (groupYouNotBelongsToEscapeSequence def) $ "gn" `Map.lookup` parametors
  }
    where
      def = defaultExtensionConfig
      parametors = parametorsFrom lsColors

applyEscapeSequence :: Config -> String -> String
applyEscapeSequence conf escSeq = left ++ escSeq ++ right
  where
    left = leftEscapeSequence conf
    right = rightEscapeSequence conf

nodeTypeOf :: Files.FileStatus -> NodeType
nodeTypeOf status
  | Files.isRegularFile status = regularFileNodeTypeOf status
  | Files.isDirectory status = directoryNodeTypeOf status
  | Files.isSymbolicLink status = SymbolicLink
  | Files.isNamedPipe status = NamedPipe
  | Files.isSocket status = Socket
  | Files.isBlockDevice status = BlockDevise
  | Files.isCharacterDevice status = CharDevise
  | otherwise = Orphan

regularFileNodeTypeOf :: Files.FileStatus -> NodeType
regularFileNodeTypeOf status
  | isSetuidMode mode = Setuid
  | isSetgidMode mode = Setgid
  | isExecutableMode mode = Executable
  | otherwise = File
    where
      mode = Files.fileMode status

directoryNodeTypeOf :: Files.FileStatus -> NodeType
directoryNodeTypeOf status
  | isStickyOtherWrite mode = StickyOtherWritable
  | isOtherWriteMode mode = OtherWritable
  | isStickyMode mode = Sticky
  | otherwise = Directory
    where
      mode = Files.fileMode status

colorizedNodeName :: Config -> Node.NodeInfo -> String
colorizedNodeName conf nd = start ++ name ++ end
    where
      left = leftEscapeSequence conf
      right = rightEscapeSequence conf
      end = left ++ right
      start = left ++ escSec ++ right
      name = nodeName nd
      escSec = lookupEscSec conf nd

lookupEscSec :: Config -> Node.NodeInfo -> String
lookupEscSec conf nd = case nodeTypeOf $ Node.nodeInfoStatus nd of
  Directory -> directoryEscapeSequence conf
  SymbolicLink -> symlinkEscSeq
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
  Orphan -> orphanedSymlinkEscSeq
  where
    orphanedSymlinkEscSeq = orphanedSymlinkEscapeSequence conf
    symlinkEscSeq = case nd of
                      Node.FileInfo {} -> lookupSymlinkEscSeq
                      Node.LinkInfo {} -> lookupSymlinkEscSeq
                      Node.OrphanedLinkInfo {} -> orphanedSymlinkEscSeq
    lookupSymlinkEscSeq = if symlinkEscSeq' == "target"
                             then lookupEscSec conf $ Node.FileInfo
                               { Node.getFilePath = Node.getDestPath nd
                               , Node.getFileStatus = Node.getDestStatus nd
                               }
                             else symlinkEscSeq'
    symlinkEscSeq' = symlinkEscapeSequence conf

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

nodeName :: Node.NodeInfo -> String
nodeName node = Posix.takeFileName name
  where
    name = case node of
             Node.FileInfo {} -> Node.getFilePath node
             Node.LinkInfo {} -> Node.getLinkPath node
             Node.OrphanedLinkInfo {} -> Node.getOrphanedLinkPath node

hasFileMode :: Types.FileMode -> Types.FileMode -> Bool
hasFileMode x y = x == Files.intersectFileModes x y

isOwnerExecuteMode :: Types.FileMode -> Bool
isOwnerExecuteMode = hasFileMode Files.ownerExecuteMode

isGroupExecuteMode :: Types.FileMode -> Bool
isGroupExecuteMode = hasFileMode Files.groupExecuteMode

isOtherWriteMode :: Types.FileMode -> Bool
isOtherWriteMode = hasFileMode Files.otherWriteMode

isOtherExecuteMode :: Types.FileMode -> Bool
isOtherExecuteMode = hasFileMode Files.otherExecuteMode

isExecutableMode :: Types.FileMode -> Bool
isExecutableMode = or . sequence [isOwnerExecuteMode, isGroupExecuteMode, isOtherExecuteMode]

isSetuidMode :: Types.FileMode -> Bool
isSetuidMode = hasFileMode Files.setUserIDMode

isSetgidMode :: Types.FileMode -> Bool
isSetgidMode = hasFileMode Files.setGroupIDMode

isStickyMode :: Types.FileMode -> Bool
isStickyMode = hasFileMode stickyMode

isStickyOtherWrite :: Types.FileMode -> Bool
isStickyOtherWrite = hasFileMode stickyOtherWriteMode

stickyMode :: Types.FileMode
stickyMode = 548

stickyOtherWriteMode :: Types.FileMode
stickyOtherWriteMode = Files.unionFileModes stickyMode Files.otherWriteMode
