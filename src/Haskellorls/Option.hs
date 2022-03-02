module Haskellorls.Option
  ( Option (..),
    opts,
  )
where

import qualified Control.Applicative as A
import qualified Data.Text as T
import qualified Haskellorls.Color.Option as Color
import qualified Haskellorls.Depth as Depth
import qualified Haskellorls.Format.Option as Format
import qualified Haskellorls.Indicator.Option as Indicator
import qualified Haskellorls.Quote.Option as Quote
import qualified Haskellorls.Size.Option as Size
import qualified Haskellorls.Sort.Option as Sort
import qualified Haskellorls.Time.Option as Time
import qualified Options.Applicative as OA
import qualified Options.Applicative.Help.Pretty as OA
import qualified Text.Read as Read

data Option = Option
  { all :: Bool,
    almostAll :: Bool,
    author :: Bool,
    escape :: Bool,
    blockSize :: Size.BlockSize,
    ignoreBackups :: Bool,
    ctime :: Bool,
    vertical :: Bool,
    color :: Color.Colorize,
    directory :: Bool,
    noneSortExtra :: Bool,
    classify :: Bool,
    extraColor :: Bool,
    fileType :: Bool,
    format :: Format.Format,
    fullTime :: Bool,
    longWithoutOwner :: Bool,
    groupDirectoriesFirst :: Bool,
    noGroup :: Bool,
    humanReadable :: Bool,
    si :: Bool,
    dereferenceCommandLine :: Bool,
    dereferenceCommandLineSymlinkToDir :: Bool,
    hide :: String,
    icon :: Bool,
    indicatorStyle :: Indicator.IndicatorStyle,
    inode :: Bool,
    ignore :: String,
    kibibyte :: Bool,
    level :: Depth.Depth,
    long :: Bool,
    dereference :: Bool,
    fillWidth :: Bool,
    numericUidGid :: Bool,
    literal :: Bool,
    longWithoutGroup :: Bool,
    directoryIndicator :: Bool,
    hideControlChars :: Bool,
    showControlChars :: Bool,
    quoteName :: Bool,
    quotingStyle :: Quote.QuotingStyle,
    reverse :: Bool,
    recursive :: Bool,
    size :: Bool,
    sizeSort :: Bool,
    sort :: Sort.SortType,
    time :: Time.TimeType,
    timeStyle :: Time.TimeStyle,
    timeSort :: Bool,
    tabSeparator :: Bool,
    tabSize :: Int,
    tree :: Bool,
    atime :: Bool,
    noneSort :: Bool,
    naturalSort :: Bool,
    width :: Maybe Int,
    horihontal :: Bool,
    extensionSort :: Bool,
    context :: Bool,
    oneline :: Bool,
    toStdout :: Bool,
    noQuote :: Bool,
    version :: Bool,
    targets :: [FilePath]
  }

opts :: OA.ParserInfo Option
opts = OA.info (optionParser OA.<**> OA.helper) $ OA.fullDesc <> (OA.headerDoc . Just . OA.text . T.unpack) header

optionParser :: OA.Parser Option
optionParser =
  Option
    <$> allParser
    <*> almostAllParser
    <*> authorParser
    <*> escapeParser
    <*> Size.blockSizeParser
    <*> ignoreBackupsParser
    <*> ctimeParser
    <*> verticalParser
    <*> Color.colorParser
    <*> directoryParser
    <*> noneSortExtraParser
    <*> classifyParser
    <*> Color.extraColorParser
    <*> fileTypeParser
    <*> Format.formatParser
    <*> fullTimeParser
    <*> longWithoutOwnerParser
    <*> groupDirectoriesFirstParser
    <*> noGroupParser
    <*> Size.humanReadableParser
    <*> Size.siParser
    <*> dereferenceCommandLineParser
    <*> dereferenceCommandLineSymlinkToDirParser
    <*> hideParser
    <*> iconParser
    <*> Indicator.indicatorStyleParser
    <*> inodeParser
    <*> ignoreParser
    <*> kibibytesParser
    <*> levelParser
    <*> longParser
    <*> dereferenceParser
    <*> fillWidthParser
    <*> numericUidGidParser
    <*> literalParser
    <*> longWithoutGroupParser
    <*> directoryIndicatorParser
    <*> hideControlCharsParser
    <*> showControlCharsParser
    <*> quoteNameParser
    <*> Quote.quotingStyleParser
    <*> reverseParser
    <*> recursiveParser
    <*> sizeParser
    <*> Sort.sizeSortParser
    <*> Sort.sortParser
    <*> Time.timeParser
    <*> Time.timeStyleParser
    <*> Sort.timeSortParser
    <*> tabSeparatorParser
    <*> tabSizeParser
    <*> treeParser
    <*> atimeParser
    <*> Sort.noneSortParser
    <*> Sort.naturalSortParser
    <*> widthParser
    <*> horihontalParser
    <*> Sort.extensionSortParser
    <*> contextParser
    <*> onelineParser
    <*> toStdoutParser
    <*> noQuoteParser
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

levelParser :: OA.Parser Depth.Depth
levelParser =
  OA.option reader $
    OA.long "level"
      <> OA.metavar "DEPTH"
      <> OA.value Depth.makeInf
      <> OA.help "Specify how much depth drills in directory"
  where
    reader =
      OA.str >>= \s -> do
        case Read.readMaybe s >>= Depth.makeDepth of
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

treeParser :: OA.Parser Bool
treeParser =
  OA.switch $
    OA.long "tree"
      <> OA.help "Output each files with tree style layout. If combines with '-a/--all', the option is disabled forcefully and '-A/--almost-all' is enabled instead."

dereferenceParser :: OA.Parser Bool
dereferenceParser =
  OA.switch $
    OA.long "dereference"
      <> OA.short 'L'
      <> OA.help "Use symbolic link destination file instead of link itself"

dereferenceCommandLineParser :: OA.Parser Bool
dereferenceCommandLineParser =
  OA.switch $
    OA.long "dereference-command-line"
      <> OA.short 'H'
      <> OA.help "Use symbolic link destination file instead of link itself on command line arguments"

dereferenceCommandLineSymlinkToDirParser :: OA.Parser Bool
dereferenceCommandLineSymlinkToDirParser =
  OA.switch $
    OA.long "dereference-command-line-symlink-to-dir"
      <> OA.help "Use symbolic link destination file instead of link itself on command line arguments when destination is directory"

fullTimeParser :: OA.Parser Bool
fullTimeParser =
  OA.switch $
    OA.long "full-time"
      <> OA.help "Equals to specify '-l' and '--time-style=full-iso'"

groupDirectoriesFirstParser :: OA.Parser Bool
groupDirectoriesFirstParser =
  OA.switch $
    OA.long "group-directories-first"
      <> OA.help "Outputs directories firstly than files; can use with '--sort=WORD' option, but this is disabled when with '--sort=none or -U'"

verticalParser :: OA.Parser Bool
verticalParser =
  OA.switch $
    OA.short 'C'
      <> OA.help "Outputs files by columns"

horihontalParser :: OA.Parser Bool
horihontalParser =
  OA.switch $
    OA.short 'x'
      <> OA.help "Outputs files by rows instead of columns"

fillWidthParser :: OA.Parser Bool
fillWidthParser =
  OA.switch $
    OA.short 'm'
      <> OA.help "Output grid style layout which are filled with comma as possible"

ctimeParser :: OA.Parser Bool
ctimeParser =
  OA.switch $
    OA.short 'c'
      <> OA.help "Sort by ctime and show it when '-t' and long style (e.g. '-l', '-o' and so on) options are passed; sort by name and show ctime when long style option is passed; sort by ctime otherwise"

directoryParser :: OA.Parser Bool
directoryParser =
  OA.switch $
    OA.long "directory"
      <> OA.short 'd'
      <> OA.help "Treats directory as normal file, does not lookup contents in it"

noneSortExtraParser :: OA.Parser Bool
noneSortExtraParser =
  OA.switch $
    OA.short 'f'
      <> OA.help "Do not sort and enable '-a' and '--color=disable' options"

atimeParser :: OA.Parser Bool
atimeParser =
  OA.switch $
    OA.short 'u'
      <> OA.help "Sort by access time and show it when '-t' and long style (e.g. '-l', '-o' and so on) options are passed; sort by name and show access time when long style option is passed; sort by access time otherwise"

tabSizeParser :: OA.Parser Int
tabSizeParser =
  OA.option reader $
    OA.long "tabsize"
      <> OA.short 'T'
      <> OA.metavar "COLS"
      <> OA.value 8
      <> OA.help "Specify tab stop size; default is 8"
  where
    reader =
      OA.str >>= \cols ->
        case Read.readMaybe cols of
          Just n -> pure n
          Nothing -> OA.readerError "COLS must be a natural number"

tabSeparatorParser :: OA.Parser Bool
tabSeparatorParser =
  OA.switch $
    OA.long "tab-separator"
      <> OA.help "Use tab charactors and some spaces to separate grid divisions when grid layout style; like dir"

quoteNameParser :: OA.Parser Bool
quoteNameParser =
  OA.switch $
    OA.long "quote-name"
      <> OA.short 'Q'
      <> OA.help "Quote file name and link name with double quote (\")"

noQuoteParser :: OA.Parser Bool
noQuoteParser =
  OA.switch $
    OA.long "no-quote"
      <> OA.internal

contextParser :: OA.Parser Bool
contextParser =
  OA.switch $
    OA.long "context"
      <> OA.short 'Z'
      <> OA.help "Output security context information of each files"

literalParser :: OA.Parser Bool
literalParser =
  OA.switch $
    OA.long "literal"
      <> OA.short 'N'
      <> OA.help "Output file name and link name without quoting; and replace all non printable characters to '?'"

toStdoutParser :: OA.Parser Bool
toStdoutParser =
  OA.switch $
    OA.long "to-stdout"
      <> OA.internal

hideControlCharsParser :: OA.Parser Bool
hideControlCharsParser =
  OA.switch $
    OA.long "hide-control-chars"
      <> OA.short 'q'
      <> OA.help "Output '?' instead of control characters"

showControlCharsParser :: OA.Parser Bool
showControlCharsParser =
  OA.switch $
    OA.long "show-control-chars"
      <> OA.help "Output control characters 'as is'"

kibibytesParser :: OA.Parser Bool
kibibytesParser =
  OA.switch $
    OA.long "kibibytes"
      <> OA.short 'k'
      <> OA.help "Use 1024 byte as one block size; This overrides values of some environment variables such as 'LS_BLOCK_SIZE' and 'BLOCK_SIZE'"

escapeParser :: OA.Parser Bool
escapeParser =
  OA.switch $
    OA.long "escape"
      <> OA.short 'b'
      <> OA.help "Escape file name and link name by C lang style"

versionParser :: OA.Parser Bool
versionParser =
  OA.switch $
    OA.long "version"
      <> OA.help "Show version info"

argParser :: OA.Parser [String]
argParser = A.many . OA.strArgument $ OA.metavar "[FILE]..."

header :: T.Text
header =
  T.unlines
    [ " _   _           _        _ _            _     ",
      "| | | |         | |      | | |          | |    ",
      "| |_| | __ _ ___| | _____| | | ___  _ __| |___ ",
      "|  _  |/ _` / __| |/ / _ \\ | |/ _ \\| '__| / __|",
      "| | | | (_| \\__ \\   <  __/ | | (_) | |  | \\__ \\",
      "\\_| |_/\\__,_|___/_|\\_\\___|_|_|\\___/|_|  |_|___/",
      "",
      "Haskellorls = Haskell color ls"
    ]
