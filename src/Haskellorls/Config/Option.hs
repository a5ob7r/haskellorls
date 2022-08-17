module Haskellorls.Config.Option
  ( Option (..),
    opts,
  )
where

import qualified Haskellorls.Config.Option.Format as Format
import qualified Haskellorls.Config.Option.Indicator as Indicator
import qualified Haskellorls.Config.Option.Quote as Quote
import qualified Haskellorls.Config.Option.Size as Size
import qualified Haskellorls.Config.Option.Sort as Sort
import qualified Haskellorls.Config.Option.Time as Time
import qualified Haskellorls.Config.Option.When as W
import Haskellorls.Data.Infinitable
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
    oDired :: Bool,
    oNoneSortExtra :: Bool,
    oClassify :: Maybe W.WHEN,
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
    oHide :: Maybe String,
    oHyperlink :: W.WHEN,
    oIcon :: Bool,
    oIndicatorStyle :: Indicator.IndicatorStyle,
    oInode :: Bool,
    oIgnore :: Maybe String,
    oKibibyte :: Bool,
    oLevel :: Infinitable Int,
    oLong :: Bool,
    oDereference :: Bool,
    oFillWidth :: Bool,
    oNumericUidGid :: Bool,
    oLiteral :: Bool,
    oLongWithoutGroup :: Bool,
    oDirectoryIndicator :: Bool,
    oShowControlChars :: Maybe Bool,
    oQuoteName :: Bool,
    oQuotingStyle :: Maybe Quote.QuotingStyle,
    oReverse :: Bool,
    oRecursive :: Bool,
    oSize :: Bool,
    oSizeSort :: Bool,
    oSort :: Sort.SortType,
    oTime :: Time.TimeType,
    oTimeStyle :: Maybe Time.TimeStyle,
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
  where
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

optionParser :: Parser Option
optionParser =
  Option
    <$> switch do
      long "all"
        <> short 'a'
        <> help "Output hidden files contain '.' and '..'"
    <*> switch do
      long "almost-all"
        <> short 'A'
        <> help "Output hidden files doesn't contain '.' and '..'"
    <*> switch do
      long "author"
        <> help "Output file author, but this is equal to file owner (for compatibiliy to GNU ls)"
    <*> switch do
      long "escape"
        <> short 'b'
        <> help "Escape file name and link name by C lang style"
    <*> Size.blockSizeParser
    <*> switch do
      long "ignore-backups"
        <> short 'B'
        <> help "Ignore backup files which have a suffix with '~'"
    <*> switch do
      short 'c'
        <> help "Sort by ctime and show it when '-t' and long style (e.g. '-l', '-o' and so on) options are passed; sort by name and show ctime when long style option is passed; sort by ctime otherwise"
    <*> switch do
      short 'C'
        <> help "Outputs files by columns"
    <*> do
      let noArg = flag' W.ALWAYS $ long "color"
          withArg =
            option W.reader $
              long "color"
                <> metavar "WHEN"
                <> value W.NEVER
                <> help "When use output with color. If omits =WHEN, then the value assumes 'always'. (default is 'never')."
                <> completeWith ["never", "always", "auto"]
       in noArg <|> withArg
    <*> switch do
      long "directory"
        <> short 'd'
        <> help "Treats directory as normal file, does not lookup contents in it"
    <*> switch do
      long "dired"
        <> short 'D'
        <> help "Append metadata for Emacs' dired mode."
    <*> switch do
      short 'f'
        <> help "Do not sort and enable '-a' and '--color=disable' options"
    <*> do
      let noArg = flag' (Just W.ALWAYS) $ long "classify" <> short 'F'
          withArg =
            option (Just <$> W.reader) $
              long "classify"
                <> value Nothing
                <> metavar "WHEN"
                <> help "Append a indicator follows filename"
                <> completeWith ["never", "always", "auto"]
       in noArg <|> withArg
    <*> switch do
      long "extra-color"
        <> help "Enable extra coloring which is incompatible for GNU ls."
    <*> switch do
      long "file-type"
        <> help "Append indicators without '*'"
    <*> Format.formatParser
    <*> switch do
      long "full-time"
        <> help "Equals to specify '-l' and '--time-style=full-iso'"
    <*> switch do
      short 'g'
        <> help "Enable long layout without file owner"
    <*> switch do
      long "group-directories-first"
        <> help "Outputs directories firstly than files; can use with '--sort=WORD' option, but this is disabled when with '--sort=none or -U'"
    <*> switch do
      long "no-group"
        <> short 'G'
        <> help "Hide file group field"
    <*> switch do
      long "human-readable"
        <> short 'h'
        <> help "Enable human readable output about file size (e.g. 1K, 23M)"
    <*> switch do
      long "si"
        <> help "Use 1000 as file size power instead of 1024"
    <*> switch do
      long "dereference-command-line"
        <> short 'H'
        <> help "Use symbolic link destination file instead of link itself on command line arguments"
    <*> switch do
      long "dereference-command-line-symlink-to-dir"
        <> help "Use symbolic link destination file instead of link itself on command line arguments when destination is directory"
    <*> option (Just <$> str) do
      long "hide"
        <> metavar "PATTERN"
        <> value Nothing
        <> help "Don't output file infos if the file name matches to glob 'PATTERN', but this option is ignored when -a or -A is passed at same time"
    <*> do
      let noArg = flag' W.ALWAYS $ long "hyperlink"
          withArg =
            option W.reader $
              long "hyperlink"
                <> metavar "WHEN"
                <> value W.NEVER
                <> help "When embed the hyperlink to the file into the filename."
                <> completeWith ["never", "always", "auto"]
       in noArg <|> withArg
    <*> switch do
      long "icons"
        <> help "Output icon for each files"
    <*> Indicator.indicatorStyleParser
    <*> switch do
      long "inode"
        <> short 'i'
        <> help "Output inode number about each files"
    <*> option (Just <$> str) do
      long "ignore"
        <> short 'I'
        <> metavar "PATTERN"
        <> value Nothing
        <> help "Don't output file infos if the file name matches to glob 'PATTERN'"
    <*> switch do
      long "kibibytes"
        <> short 'k'
        <> help "Use 1024 byte as one block size; This overrides values of some environment variables such as 'LS_BLOCK_SIZE' and 'BLOCK_SIZE'"
    <*> option
      do
        str >>= \s -> case readMaybe s of
          Just n | n > -1 -> return $ Only n
          _ -> readerError "DEPTH must be a non-negative integer."
      do
        long "level"
          <> metavar "DEPTH"
          <> value Infinity
          <> help "Specify how much depth drills in directory"
    <*> switch do
      short 'l'
        <> help "Enable long layout which provides informative outputs about files"
    <*> switch do
      long "dereference"
        <> short 'L'
        <> help "Use symbolic link destination file instead of link itself"
    <*> switch do
      short 'm'
        <> help "Output grid style layout which are filled with comma as possible"
    <*> switch do
      long "numeric-uid-gid"
        <> short 'n'
        <> help "Output numeric uid and gid instead of alphabetical them"
    <*> switch do
      long "literal"
        <> short 'N'
        <> help "Output file name and link name without quoting; and replace all non printable characters to '?'"
    <*> switch do
      short 'o'
        <> help "Enable long layout without file group"
    <*> switch do
      short 'p'
        <> help "Append a indicator '/' to directories"
    <*> do
      let sParser =
            flag' (Just True) $
              long "show-control-chars"
                <> help "Output control characters 'as is'"
          hParser =
            flag Nothing (Just False) $
              long "hide-control-chars"
                <> short 'q'
                <> help "Output '?' instead of control characters"
       in sParser <|> hParser
    <*> switch do
      long "quote-name"
        <> short 'Q'
        <> help "Quote file name and link name with double quote (\")"
    <*> Quote.quotingStyleParser
    <*> switch do
      long "reverse"
        <> short 'r'
        <> help "Reverse outputs order"
    <*> switch do
      long "recursive"
        <> short 'R'
        <> help "Output infos about files in sub directories recursively"
    <*> switch do
      long "size"
        <> short 's'
        <> help "Output allocated block size of each files"
    <*> switch do
      short 'S'
        <> help "Size sort, largest first"
    <*> Sort.sortParser
    <*> Time.timeParser
    <*> Time.timeStyleParser
    <*> switch do
      short 't'
        <> help "Time sort, newest first"
    <*> switch do
      long "tab-separator"
        <> help "Use tab charactors and some spaces to separate grid divisions when grid layout style; like dir"
    <*> option
      do str >>= maybe (readerError "COLS must be a natural number") return . readMaybe
      do
        long "tabsize"
          <> short 'T'
          <> metavar "COLS"
          <> value 8
          <> help "Specify tab stop size; default is 8"
    <*> switch do
      long "tree"
        <> help "Output each files with tree style layout. If combines with '-a/--all', the option is disabled forcefully and '-A/--almost-all' is enabled instead."
    <*> switch do
      short 'u'
        <> help "Sort by access time and show it when '-t' and long style (e.g. '-l', '-o' and so on) options are passed; sort by name and show access time when long style option is passed; sort by access time otherwise"
    <*> switch do
      short 'U'
        <> help "Do not sort"
    <*> switch do
      short 'v'
        <> help "Natural sort"
    <*> option
      do auto >>= \n -> if n >= 0 then return $ Just n else readerError "COLS must be a natural number"
      do
        long "width"
          <> short 'w'
          <> metavar "COLS"
          <> value Nothing
          <> help "Specify output width. Assumes infinity width if 0."
    <*> switch do
      short 'x'
        <> help "Outputs files by rows instead of columns"
    <*> switch do
      short 'X'
        <> help "Alphabetical sort by file extension"
    <*> switch do
      long "context"
        <> short 'Z'
        <> help "Output security context information of each files"
    <*> switch do
      long "zero"
        <> help "Each line is terminated with NUL, not newline."
    <*> switch do
      short '1'
        <> help "Enable oneline layout which outputs one file by one line"
    <*> switch do
      long "version"
        <> help "Show version info"
    <*> do
      many . strArgument $
        metavar "[FILE]..."
          <> action "file"
