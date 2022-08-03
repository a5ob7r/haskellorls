module Haskellorls.Config.Option
  ( Option (..),
    opts,
  )
where

import qualified Haskellorls.Config.Option.Color as Color
import qualified Haskellorls.Config.Option.Format as Format
import qualified Haskellorls.Config.Option.Hyperlink as Hyperlink
import qualified Haskellorls.Config.Option.Indicator as Indicator
import qualified Haskellorls.Config.Option.Quote as Quote
import qualified Haskellorls.Config.Option.Size as Size
import qualified Haskellorls.Config.Option.Sort as Sort
import qualified Haskellorls.Config.Option.Time as Time
import qualified Haskellorls.Config.When as W
import qualified Haskellorls.Depth as Depth
import Options.Applicative hiding (header)
import Options.Applicative.Help.Pretty
import Text.Read

-- | An interface type for the option parser.
data Option = Option
  { oAll :: Bool,
    oAlmostAll :: Bool,
    oAuthor :: Bool,
    oEscape :: Bool,
    oBlockSize :: Size.BlockSize,
    oIgnoreBackups :: Bool,
    oCtime :: Bool,
    oVertical :: Bool,
    oColor :: W.WHEN,
    oDirectory :: Bool,
    oNoneSortExtra :: Bool,
    oClassify :: W.WHEN,
    oExtraColor :: Bool,
    oFileType :: Bool,
    oFormat :: Maybe Format.Format,
    oFullTime :: Bool,
    oLongWithoutOwner :: Bool,
    oGroupDirectoriesFirst :: Bool,
    oNoGroup :: Bool,
    oHumanReadable :: Bool,
    oSi :: Bool,
    oDereferenceCommandLine :: Bool,
    oDereferenceCommandLineSymlinkToDir :: Bool,
    oHide :: String,
    oHyperlink :: W.WHEN,
    oIcon :: Bool,
    oIndicatorStyle :: Indicator.IndicatorStyle,
    oInode :: Bool,
    oIgnore :: String,
    oKibibyte :: Bool,
    oLevel :: Depth.Depth,
    oLong :: Bool,
    oDereference :: Bool,
    oFillWidth :: Bool,
    oNumericUidGid :: Bool,
    oLiteral :: Bool,
    oLongWithoutGroup :: Bool,
    oDirectoryIndicator :: Bool,
    oHideControlChars :: Bool,
    oShowControlChars :: Bool,
    oQuoteName :: Bool,
    oQuotingStyle :: Quote.QuotingStyle,
    oReverse :: Bool,
    oRecursive :: Bool,
    oSize :: Bool,
    oSizeSort :: Bool,
    oSort :: Sort.SortType,
    oTime :: Time.TimeType,
    oTimeStyle :: Time.TimeStyle,
    oTimeSort :: Bool,
    oTabSeparator :: Bool,
    oTabSize :: Int,
    oTree :: Bool,
    oAtime :: Bool,
    oNoneSort :: Bool,
    oNaturalSort :: Bool,
    oWidth :: Maybe Int,
    oHorihontal :: Bool,
    oExtensionSort :: Bool,
    oContext :: Bool,
    oZero :: Bool,
    oOneline :: Bool,
    oVersion :: Bool,
    oTargets :: [FilePath]
  }

opts :: ParserInfo Option
opts = info (optionParser <**> helper) $ fullDesc <> (headerDoc . Just . text) header

optionParser :: Parser Option
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
    <*> Indicator.classifyParser
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
    <*> Hyperlink.hyperlinkParser
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
    <*> zeroParser
    <*> onelineParser
    <*> versionParser
    <*> argParser

longParser :: Parser Bool
longParser =
  switch $
    short 'l'
      <> help "Enable long layout which provides informative outputs about files"

allParser :: Parser Bool
allParser =
  switch $
    short 'a'
      <> long "all"
      <> help "Output hidden files contain '.' and '..'"

almostAllParser :: Parser Bool
almostAllParser =
  switch $
    short 'A'
      <> long "almost-all"
      <> help "Output hidden files doesn't contain '.' and '..'"

reverseParser :: Parser Bool
reverseParser =
  switch $
    short 'r'
      <> long "reverse"
      <> help "Reverse outputs order"

zeroParser :: Parser Bool
zeroParser = switch $ long "zero" <> help "Each line is terminated with NUL, not newline."

onelineParser :: Parser Bool
onelineParser =
  switch $
    short '1'
      <> help "Enable oneline layout which outputs one file by one line"

noGroupParser :: Parser Bool
noGroupParser =
  switch $
    short 'G'
      <> long "no-group"
      <> help "Hide file group field"

longWithoutGroupParser :: Parser Bool
longWithoutGroupParser =
  switch $
    short 'o'
      <> help "Enable long layout without file group"

longWithoutOwnerParser :: Parser Bool
longWithoutOwnerParser =
  switch $
    short 'g'
      <> help "Enable long layout without file owner"

widthParser :: Parser (Maybe Int)
widthParser =
  option reader $
    long "width"
      <> short 'w'
      <> metavar "COLS"
      <> help "Specify output width. Assumes infinity width if 0."
      <> value Nothing
  where
    reader =
      auto >>= \n ->
        if n >= 0
          then return $ Just n
          else readerError "COLS must be a natural number"

inodeParser :: Parser Bool
inodeParser =
  switch $
    long "inode"
      <> short 'i'
      <> help "Output inode number about each files"

directoryIndicatorParser :: Parser Bool
directoryIndicatorParser =
  switch $
    short 'p'
      <> help "Append a indicator '/' to directories"

fileTypeParser :: Parser Bool
fileTypeParser =
  switch $
    long "file-type"
      <> help "Append indicators without '*'"

ignoreBackupsParser :: Parser Bool
ignoreBackupsParser =
  switch $
    long "ignore-backups"
      <> short 'B'
      <> help "Ignore backup files which have a suffix with '~'"

numericUidGidParser :: Parser Bool
numericUidGidParser =
  switch $
    long "numeric-uid-gid"
      <> short 'n'
      <> help "Output numeric uid and gid instead of alphabetical them"

ignoreParser :: Parser String
ignoreParser =
  strOption $
    long "ignore"
      <> short 'I'
      <> metavar "PATTERN"
      <> value ""
      <> help "Don't output file infos if the file name matches to glob 'PATTERN'"

hideParser :: Parser String
hideParser =
  strOption $
    long "hide"
      <> metavar "PATTERN"
      <> value ""
      <> help "Don't output file infos if the file name matches to glob 'PATTERN', but this option is ignored when -a or -A is passed at same time"

recursiveParser :: Parser Bool
recursiveParser =
  switch $
    long "recursive"
      <> short 'R'
      <> help "Output infos about files in sub directories recursively"

levelParser :: Parser Depth.Depth
levelParser =
  option reader $
    long "level"
      <> metavar "DEPTH"
      <> value Depth.makeInf
      <> help "Specify how much depth drills in directory"
  where
    reader =
      str >>= \s -> do
        case readMaybe s >>= Depth.makeDepth of
          Just d -> return d
          _ -> readerError "Acceptable value is only natural number"

authorParser :: Parser Bool
authorParser =
  switch $
    long "author"
      <> help "Output file author, but this is equal to file owner (for compatibiliy to GNU ls)"

sizeParser :: Parser Bool
sizeParser =
  switch $
    long "size"
      <> short 's'
      <> help "Output allocated block size of each files"

iconParser :: Parser Bool
iconParser =
  switch $
    long "icons"
      <> help "Output icon for each files"

treeParser :: Parser Bool
treeParser =
  switch $
    long "tree"
      <> help "Output each files with tree style layout. If combines with '-a/--all', the option is disabled forcefully and '-A/--almost-all' is enabled instead."

dereferenceParser :: Parser Bool
dereferenceParser =
  switch $
    long "dereference"
      <> short 'L'
      <> help "Use symbolic link destination file instead of link itself"

dereferenceCommandLineParser :: Parser Bool
dereferenceCommandLineParser =
  switch $
    long "dereference-command-line"
      <> short 'H'
      <> help "Use symbolic link destination file instead of link itself on command line arguments"

dereferenceCommandLineSymlinkToDirParser :: Parser Bool
dereferenceCommandLineSymlinkToDirParser =
  switch $
    long "dereference-command-line-symlink-to-dir"
      <> help "Use symbolic link destination file instead of link itself on command line arguments when destination is directory"

fullTimeParser :: Parser Bool
fullTimeParser =
  switch $
    long "full-time"
      <> help "Equals to specify '-l' and '--time-style=full-iso'"

groupDirectoriesFirstParser :: Parser Bool
groupDirectoriesFirstParser =
  switch $
    long "group-directories-first"
      <> help "Outputs directories firstly than files; can use with '--sort=WORD' option, but this is disabled when with '--sort=none or -U'"

verticalParser :: Parser Bool
verticalParser =
  switch $
    short 'C'
      <> help "Outputs files by columns"

horihontalParser :: Parser Bool
horihontalParser =
  switch $
    short 'x'
      <> help "Outputs files by rows instead of columns"

fillWidthParser :: Parser Bool
fillWidthParser =
  switch $
    short 'm'
      <> help "Output grid style layout which are filled with comma as possible"

ctimeParser :: Parser Bool
ctimeParser =
  switch $
    short 'c'
      <> help "Sort by ctime and show it when '-t' and long style (e.g. '-l', '-o' and so on) options are passed; sort by name and show ctime when long style option is passed; sort by ctime otherwise"

directoryParser :: Parser Bool
directoryParser =
  switch $
    long "directory"
      <> short 'd'
      <> help "Treats directory as normal file, does not lookup contents in it"

noneSortExtraParser :: Parser Bool
noneSortExtraParser =
  switch $
    short 'f'
      <> help "Do not sort and enable '-a' and '--color=disable' options"

atimeParser :: Parser Bool
atimeParser =
  switch $
    short 'u'
      <> help "Sort by access time and show it when '-t' and long style (e.g. '-l', '-o' and so on) options are passed; sort by name and show access time when long style option is passed; sort by access time otherwise"

tabSizeParser :: Parser Int
tabSizeParser =
  option reader $
    long "tabsize"
      <> short 'T'
      <> metavar "COLS"
      <> value 8
      <> help "Specify tab stop size; default is 8"
  where
    reader =
      str >>= \cols ->
        case readMaybe cols of
          Just n -> pure n
          Nothing -> readerError "COLS must be a natural number"

tabSeparatorParser :: Parser Bool
tabSeparatorParser =
  switch $
    long "tab-separator"
      <> help "Use tab charactors and some spaces to separate grid divisions when grid layout style; like dir"

quoteNameParser :: Parser Bool
quoteNameParser =
  switch $
    long "quote-name"
      <> short 'Q'
      <> help "Quote file name and link name with double quote (\")"

contextParser :: Parser Bool
contextParser =
  switch $
    long "context"
      <> short 'Z'
      <> help "Output security context information of each files"

literalParser :: Parser Bool
literalParser =
  switch $
    long "literal"
      <> short 'N'
      <> help "Output file name and link name without quoting; and replace all non printable characters to '?'"

hideControlCharsParser :: Parser Bool
hideControlCharsParser =
  switch $
    long "hide-control-chars"
      <> short 'q'
      <> help "Output '?' instead of control characters"

showControlCharsParser :: Parser Bool
showControlCharsParser =
  switch $
    long "show-control-chars"
      <> help "Output control characters 'as is'"

kibibytesParser :: Parser Bool
kibibytesParser =
  switch $
    long "kibibytes"
      <> short 'k'
      <> help "Use 1024 byte as one block size; This overrides values of some environment variables such as 'LS_BLOCK_SIZE' and 'BLOCK_SIZE'"

escapeParser :: Parser Bool
escapeParser =
  switch $
    long "escape"
      <> short 'b'
      <> help "Escape file name and link name by C lang style"

versionParser :: Parser Bool
versionParser =
  switch $
    long "version"
      <> help "Show version info"

argParser :: Parser [String]
argParser = many . strArgument $ metavar "[FILE]..." <> action "file"

header :: String
header =
  unlines
    [ " _   _           _        _ _            _     ",
      "| | | |         | |      | | |          | |    ",
      "| |_| | __ _ ___| | _____| | | ___  _ __| |___ ",
      "|  _  |/ _` / __| |/ / _ \\ | |/ _ \\| '__| / __|",
      "| | | | (_| \\__ \\   <  __/ | | (_) | |  | \\__ \\",
      "\\_| |_/\\__,_|___/_|\\_\\___|_|_|\\___/|_|  |_|___/",
      "",
      "Haskellorls = Haskell color ls"
    ]
