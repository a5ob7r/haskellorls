module Haskellorls.Option
  ( Option (..),
    ColorOpt (..),
    IndicatorStyle (..),
    opts,
  )
where

import qualified Control.Applicative as A
import qualified Options.Applicative as OA
import qualified Haskellorls.Size.Option as Size

data Option = Option
  { color :: ColorOpt,
    extraColor :: Bool,
    long :: Bool,
    si :: Bool,
    humanReadable :: Bool,
    blockSize :: Size.BlockSize,
    time :: String,
    timeStyle :: String,
    all :: Bool,
    almostAll :: Bool,
    sort :: String,
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
    indicatorStyle :: IndicatorStyle,
    ignoreBackups :: Bool,
    numericUidGid :: Bool,
    ignore :: String,
    hide :: String,
    version :: Bool,
    targets :: [FilePath]
  }

data ColorOpt = NEVER | ALWAYS | AUTO
  deriving (Show)

data IndicatorStyle
  = IndicatorNone
  | IndicatorFiletype
  | IndicatorSlash
  | IndicatorClassify
  deriving (Eq, Ord)

opts :: OA.ParserInfo Option
opts =
  OA.info (optionParser OA.<**> OA.helper) $
    OA.fullDesc
      <> OA.progDesc "Haskellorls = Haskell color ls"

optionParser :: OA.Parser Option
optionParser =
  Option
    <$> colorParser
    <*> extraColorParser
    <*> longParser
    <*> Size.siParser
    <*> Size.humanReadableParser
    <*> Size.blockSizeParser
    <*> timeParser
    <*> timeStyleParser
    <*> allParser
    <*> almostAllParser
    <*> sortParser
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
    <*> indicatorStyleParser
    <*> ignoreBackupsParser
    <*> numericUidGidParser
    <*> ignoreParser
    <*> hideParser
    <*> versionParser
    <*> argParser

colorParser :: OA.Parser ColorOpt
colorParser =
  OA.option parseColorOpt $
    OA.long "color"
      <> OA.metavar "WHEN"
      <> OA.value NEVER
      <> OA.help "When use output with color (default is 'never')"

parseColorOpt :: OA.ReadM ColorOpt
parseColorOpt = OA.str >>= f
  where
    f s = case s of
      "never" -> return NEVER
      "always" -> return ALWAYS
      "auto" -> return AUTO
      _ -> OA.readerError "Only never, always or auto"

extraColorParser :: OA.Parser Bool
extraColorParser =
  OA.switch $
    OA.long "extra-color"
      <> OA.help "Enable extra coloring which is incompatible for GNU ls."

longParser :: OA.Parser Bool
longParser =
  OA.switch $
    OA.short 'l'
      <> OA.help "Enable long layout which provides informative outputs about files"

timeParser :: OA.Parser String
timeParser =
  OA.strOption $
    OA.long "time"
      <> OA.metavar "WORD"
      <> OA.value ""
      <> OA.help "Specify a time kind which is used as file's time attribute"

timeStyleParser :: OA.Parser String
timeStyleParser =
  OA.strOption $
    OA.long "time-style"
      <> OA.metavar "TYPE_STYLE"
      <> OA.value ""
      <> OA.help "Specify time output format"

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

sortParser :: OA.Parser String
sortParser =
  OA.strOption $
    OA.long "sort"
      <> OA.metavar "WORD"
      <> OA.value "name"
      <> OA.help "Specify an attribute to sort outputs"

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
  OA.option widthReader $
    OA.long "width"
      <> OA.short 'w'
      <> OA.metavar "COLS"
      <> OA.help "Specify output width. Assumes infinity width if 0."
      <> OA.value Nothing

widthReader :: OA.ReadM (Maybe Int)
widthReader = OA.auto >>= reader
  where
    reader n
      | n >= 0 = return $ Just n
      | otherwise = OA.readerError "COLS must be a natural number"

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

indicatorStyleParser :: OA.Parser IndicatorStyle
indicatorStyleParser =
  OA.option indicatorStyleReader $
    OA.long "indicator-style"
      <> OA.metavar "WORD"
      <> OA.value IndicatorNone
      <> OA.help "Specify indicator style, 'none', 'slash', 'file-tyep' and 'classify'"

indicatorStyleReader :: OA.ReadM IndicatorStyle
indicatorStyleReader = OA.auto >>= reader
  where
    reader s = case s of
      "none" -> return IndicatorNone
      "slash" -> return IndicatorSlash
      "file-type" -> return IndicatorFiletype
      "classify" -> return IndicatorClassify
      _ -> OA.readerError "Avairable values are only 'none', 'slash', 'file-tyep' and 'classify'"

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

versionParser :: OA.Parser Bool
versionParser =
  OA.switch $
    OA.long "version"
      <> OA.help "Show version info"

argParser :: OA.Parser [String]
argParser = A.many . OA.strArgument $ OA.metavar "[FILE]..."
