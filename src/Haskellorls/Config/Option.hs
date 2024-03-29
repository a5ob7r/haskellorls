module Haskellorls.Config.Option
  ( Option (..),
    opts,
  )
where

import Data.Version (showVersion)
import Haskellorls.Config.Format (Format)
import Haskellorls.Config.Indicator (IndicatorStyle (..))
import Haskellorls.Config.Quote (QuotingStyle)
import Haskellorls.Config.Size (BlockSize, BlockSizeMod)
import Haskellorls.Config.Sort (SortType (..))
import Haskellorls.Config.TimeStyle (TimeStyle)
import Haskellorls.Config.TimeType (TimeType (..))
import Haskellorls.Config.When qualified as W
import Haskellorls.Data.Infinitable (Infinitable (..))
import Options.Applicative hiding (header)
import Options.Applicative.Help.Pretty
import Paths_haskellorls (version)
import Text.Read (readMaybe)
import Witch (TryFromException (..), tryFrom)

-- | An interface type for the option parser.
data Option = Option
  { oAll :: Bool,
    oAlmostAll :: Bool,
    oAuthor :: Bool,
    oEscape :: Bool,
    oBlockSize :: Maybe (BlockSizeMod BlockSize),
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
    oFormat :: Maybe Format,
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
    oIndicatorStyle :: IndicatorStyle,
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
    oQuotingStyle :: Maybe QuotingStyle,
    oReverse :: Bool,
    oRecursive :: Bool,
    oSize :: Bool,
    oSizeSort :: Bool,
    oSort :: SortType,
    oTime :: TimeType,
    oTimeStyle :: Maybe TimeStyle,
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
    oTargets :: [FilePath]
  }

opts :: ParserInfo Option
opts = info parser $ fullDesc <> (headerDoc . Just . pretty) header
  where
    parser = optionParser <**> simpleVersioner (showVersion version) <**> helper
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
    <*> option
      do str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid block size format." show e) (return . Just) . tryFrom @String
      do
        long "block-size"
          <> metavar "SIZE"
          <> value Nothing
          <> help "Specify size unit when output file size"
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
            option whenReader $
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
            option (Just <$> whenReader) $
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
    <*> option
      do str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid format style format." show e) (return . Just) . tryFrom @String
      do
        long "format"
          <> metavar "WORD"
          <> value Nothing
          <> completeWith ["across", "commas", "horizontal", "long", "single-column", "verbose", "vertical"]
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
            option whenReader $
              long "hyperlink"
                <> metavar "WHEN"
                <> value W.NEVER
                <> help "When embed the hyperlink to the file into the filename."
                <> completeWith ["never", "always", "auto"]
       in noArg <|> withArg
    <*> switch do
      long "icons"
        <> help "Output icon for each files"
    <*> option
      do str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid indicator style format." show e) return . tryFrom @String
      do
        long "indicator-style"
          <> metavar "WORD"
          <> value IndicatorNone
          <> help "Specify indicator style, 'none', 'slash', 'file-tyep' and 'classify'"
          <> completeWith ["none", "slash", "file-type", "classify"]
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
    <*> option
      do str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid quoting style formats." show e) (return . Just) . tryFrom @String
      do
        long "quoting-style"
          <> metavar "WORD"
          <> value Nothing
          <> help "Specify file name and link name quoting style; this also effects to file name and link name escape style"
          <> completeWith ["literal", "shell", "shell-always", "shell-escape", "shell-escape-always", "c", "escape", "clocale", "locale"]
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
    <*> option
      do str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid sort type format." show e) return . tryFrom @String
      do
        long "sort"
          <> metavar "WORD"
          <> value NAME
          <> help "Specify an attribute to sort outputs"
          <> completeWith ["none", "size", "time", "version", "extension"]
    <*> option
      do str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid time kind formats." show e) return . tryFrom @String
      do
        long "time"
          <> metavar "WORD"
          <> value MODIFICATION
          <> help "Specify a time kind which is used as file's time attribute"
          <> completeWith ["atime", "access", "use", "ctime", "status"]
    <*> option
      do str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid time style format." show e) (return . Just) . tryFrom @String
      do
        long "time-style"
          <> metavar "TYPE_STYLE"
          <> value Nothing
          <> help "Specify time output format"
          <> completeWith ["full-iso", "posix-full-iso", "long-iso", "posix-long-iso", "iso", "posix-iso", "locale", "posix-locale"]
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
    <*> do
      many . strArgument $
        metavar "[FILE]..."
          <> action "file"
  where
    whenReader = str >>= either (\(TryFromException _ e) -> readerError $ maybe "Invalid WHEN format." show e) return . tryFrom @String
