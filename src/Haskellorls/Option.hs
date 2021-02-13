{-# LANGUAGE LambdaCase #-}

module Haskellorls.Option
  ( Option (..),
    opts,
  )
where

import qualified Control.Applicative as A
import qualified Haskellorls.Color.Option as Color
import qualified Haskellorls.Indicator.Option as Indicator
import qualified Haskellorls.Size.Option as Size
import qualified Haskellorls.Sort.Option as Sort
import qualified Haskellorls.Time.Option as Time
import qualified Haskellorls.Tree as Tree
import qualified Options.Applicative as OA
import qualified Text.Read as Read

data Option = Option
  { color :: Color.Colorize,
    extraColor :: Bool,
    long :: Bool,
    si :: Bool,
    humanReadable :: Bool,
    blockSize :: Size.BlockSize,
    time :: Time.TimeType,
    timeStyle :: Time.TimeStyle,
    all :: Bool,
    almostAll :: Bool,
    sort :: Sort.SortType,
    reverse :: Bool,
    oneline :: Bool,
    noGroup :: Bool,
    longWithoutGroup :: Bool,
    longWithoutOwner :: Bool,
    width :: Maybe Int,
    inode :: Bool,
    classify :: Bool,
    directoryIndicator :: Bool,
    fileType :: Bool,
    indicatorStyle :: Indicator.IndicatorStyle,
    ignoreBackups :: Bool,
    numericUidGid :: Bool,
    ignore :: String,
    hide :: String,
    recursive :: Bool,
    level :: Tree.Depth,
    author :: Bool,
    size :: Bool,
    icon :: Bool,
    noneSort :: Bool,
    sizeSort :: Bool,
    timeSort :: Bool,
    naturalSort :: Bool,
    extensionSort :: Bool,
    version :: Bool,
    targets :: [FilePath]
  }

opts :: OA.ParserInfo Option
opts =
  OA.info (optionParser OA.<**> OA.helper) $
    OA.fullDesc
      <> OA.progDesc "Haskellorls = Haskell color ls"

optionParser :: OA.Parser Option
optionParser =
  Option
    <$> Color.colorParser
    <*> Color.extraColorParser
    <*> longParser
    <*> Size.siParser
    <*> Size.humanReadableParser
    <*> Size.blockSizeParser
    <*> Time.timeParser
    <*> Time.timeStyleParser
    <*> allParser
    <*> almostAllParser
    <*> Sort.sortParser
    <*> reverseParser
    <*> onelineParser
    <*> noGroupParser
    <*> longWithoutGroupParser
    <*> longWithoutOwnerParser
    <*> widthParser
    <*> inodeParser
    <*> classifyParser
    <*> directoryIndicatorParser
    <*> fileTypeParser
    <*> Indicator.indicatorStyleParser
    <*> ignoreBackupsParser
    <*> numericUidGidParser
    <*> ignoreParser
    <*> hideParser
    <*> recursiveParser
    <*> levelParser
    <*> authorParser
    <*> sizeParser
    <*> iconParser
    <*> Sort.noneSortParser
    <*> Sort.sizeSortParser
    <*> Sort.timeSortParser
    <*> Sort.naturalSortParser
    <*> Sort.extensionSortParser
    <*> versionParser
    <*> argParser

longParser :: OA.Parser Bool
longParser =
  OA.switch $
    OA.short 'l'
      <> OA.help "Enable long layout which provides informative outputs about files"

allParser :: OA.Parser Bool
allParser =
  OA.switch $
    OA.short 'a'
      <> OA.long "all"
      <> OA.help "Output hidden files contain '.' and '..'"

almostAllParser :: OA.Parser Bool
almostAllParser =
  OA.switch $
    OA.short 'A'
      <> OA.long "almost-all"
      <> OA.help "Output hidden files doesn't contain '.' and '..'"

reverseParser :: OA.Parser Bool
reverseParser =
  OA.switch $
    OA.short 'r'
      <> OA.long "reverse"
      <> OA.help "Reverse outputs order"

onelineParser :: OA.Parser Bool
onelineParser =
  OA.switch $
    OA.short '1'
      <> OA.help "Enable oneline layout which outputs one file by one line"

noGroupParser :: OA.Parser Bool
noGroupParser =
  OA.switch $
    OA.short 'G'
      <> OA.long "no-group"
      <> OA.help "Hide file group field"

longWithoutGroupParser :: OA.Parser Bool
longWithoutGroupParser =
  OA.switch $
    OA.short 'o'
      <> OA.help "Enable long layout without file group"

longWithoutOwnerParser :: OA.Parser Bool
longWithoutOwnerParser =
  OA.switch $
    OA.short 'g'
      <> OA.help "Enable long layout without file owner"

widthParser :: OA.Parser (Maybe Int)
widthParser =
  OA.option reader $
    OA.long "width"
      <> OA.short 'w'
      <> OA.metavar "COLS"
      <> OA.help "Specify output width. Assumes infinity width if 0."
      <> OA.value Nothing
  where
    reader =
      OA.auto >>= \n ->
        if n >= 0
          then return $ Just n
          else OA.readerError "COLS must be a natural number"

inodeParser :: OA.Parser Bool
inodeParser =
  OA.switch $
    OA.long "inode"
      <> OA.short 'i'
      <> OA.help "Output inode number about each files"

classifyParser :: OA.Parser Bool
classifyParser =
  OA.switch $
    OA.long "classify"
      <> OA.short 'F'
      <> OA.help "Append a indicator follows filename"

directoryIndicatorParser :: OA.Parser Bool
directoryIndicatorParser =
  OA.switch $
    OA.short 'p'
      <> OA.help "Append a indicator '/' to directories"

fileTypeParser :: OA.Parser Bool
fileTypeParser =
  OA.switch $
    OA.long "file-type"
      <> OA.help "Append indicators without '*'"

ignoreBackupsParser :: OA.Parser Bool
ignoreBackupsParser =
  OA.switch $
    OA.long "ignore-backups"
      <> OA.short 'B'
      <> OA.help "Ignore backup files which have a suffix with '~'"

numericUidGidParser :: OA.Parser Bool
numericUidGidParser =
  OA.switch $
    OA.long "numeric-uid-gid"
      <> OA.short 'n'
      <> OA.help "Output numeric uid and gid instead of alphabetical them"

ignoreParser :: OA.Parser String
ignoreParser =
  OA.strOption $
    OA.long "ignore"
      <> OA.short 'I'
      <> OA.metavar "PATTERN"
      <> OA.value ""
      <> OA.help "Don't output file infos if the file name matches to glob 'PATTERN'"

hideParser :: OA.Parser String
hideParser =
  OA.strOption $
    OA.long "hide"
      <> OA.metavar "PATTERN"
      <> OA.value ""
      <> OA.help "Don't output file infos if the file name matches to glob 'PATTERN', but this option is ignored when -a or -A is passed at same time"

recursiveParser :: OA.Parser Bool
recursiveParser =
  OA.switch $
    OA.long "recursive"
      <> OA.short 'R'
      <> OA.help "Output infos about files in sub directories recursively"

levelParser :: OA.Parser Tree.Depth
levelParser =
  OA.option reader $
    OA.long "level"
      <> OA.short 'L'
      <> OA.metavar "DEPTH"
      <> OA.value Tree.makeInf
      <> OA.help "Specify how much depth drills in directory"
  where
    reader =
      OA.str >>= \s -> do
        case Read.readMaybe s >>= Tree.makeDepth of
          Just d -> return d
          _ -> OA.readerError "Acceptable value is only natural number"

authorParser :: OA.Parser Bool
authorParser =
  OA.switch $
    OA.long "author"
      <> OA.help "Output file author, but this is equal to file owner (for compatibiliy to GNU ls)"

sizeParser :: OA.Parser Bool
sizeParser =
  OA.switch $
    OA.long "size"
      <> OA.short 's'
      <> OA.help "Output allocated block size of each files"

iconParser :: OA.Parser Bool
iconParser =
  OA.switch $
    OA.long "icons"
      <> OA.help "Output icon for each files"

versionParser :: OA.Parser Bool
versionParser =
  OA.switch $
    OA.long "version"
      <> OA.help "Show version info"

argParser :: OA.Parser [String]
argParser = A.many . OA.strArgument $ OA.metavar "[FILE]..."
