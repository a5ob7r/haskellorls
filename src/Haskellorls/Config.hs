module Haskellorls.Config
  ( Config (..),
    mkConfig,
  )
where

import Control.Applicative
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Haskellorls.Config.Environment as Env
import Haskellorls.Config.Format
import Haskellorls.Config.Indicator
import Haskellorls.Config.Listing
import Haskellorls.Config.Option
import qualified Haskellorls.Config.Option.Format as Format
import qualified Haskellorls.Config.Option.Quote as Quote
import qualified Haskellorls.Config.Option.Size as Size
import qualified Haskellorls.Config.Option.Sort as Sort
import qualified Haskellorls.Config.Option.Time as Time
import Haskellorls.Config.Quote
import Haskellorls.Config.Size (BlockSize (..))
import Haskellorls.Config.Sort
import Haskellorls.Config.Time
import qualified Haskellorls.Config.When as W
import Haskellorls.Data.Infinitable
import System.FilePath.Glob (Pattern, compile)
import System.FilePath.Posix.ByteString
import Prelude hiding (reverse)

data Config = Config
  { inode :: Bool,
    size :: Bool,
    owner :: Bool,
    group :: Bool,
    author :: Bool,
    context :: Bool,
    icon :: Bool,
    -- | Whether or not the color output mode is enabled. And if it is enabled,
    -- whether or not the extra color output mode is enabled.
    colorize :: Maybe Bool,
    hyperlink :: Bool,
    indicatorStyle :: IndicatorStyle,
    listing :: ListingStyle,
    fileSize :: Size.BlockSize,
    blockSize :: Size.BlockSize,
    ignoreBackups :: Bool,
    directory :: Bool,
    -- | When this value is 'COMMAS', every column alignment in format is
    -- disabled.
    format :: Format.Format,
    groupDirectoriesFirst :: Bool,
    si :: Bool,
    dereferenceCommandLine :: Bool,
    dereferenceCommandLineSymlinkToDir :: Bool,
    hide :: Maybe Pattern,
    ignore :: Maybe Pattern,
    level :: Infinitable Int,
    dereference :: Bool,
    numericUidGid :: Bool,
    -- | How quote and escape each filename. If no any command-line option for
    -- this option, the environment variable @QUOTING_STYLE@ is used as this
    -- option's value. If the value is unset or an invalid value, then it is
    -- ignored. In that case, this value is 'ShellEscape' when the stdout is
    -- connected to a terminl, otherwise 'Literal'.
    quotingStyle :: Quote.QuotingStyle,
    -- | Whether or not to show any control (or non-printable) charaster as is,
    -- or to replace them by @?@, when the 'quotingStyle' is 'Literal'. By
    -- default this value is 'True' unless the stdout is connected to a
    -- terminal, otherwise 'False'.
    showControlChars :: Bool,
    reverse :: Bool,
    recursive :: Bool,
    sort :: Sort.SortType,
    time :: Time.TimeType,
    timeStyle :: Maybe Time.TimeStyle,
    tabSize :: Maybe Int,
    tree :: Bool,
    width :: Int,
    zero :: Bool,
    dired :: Bool,
    currentWorkingDirectory :: RawFilePath,
    hostname :: T.Text
  }

mkConfig :: Env.Environment -> Option -> Config
mkConfig env Option {..} = Config {..}
  where
    inode = oInode
    size = oSize
    owner = not oLongWithoutOwner
    group = not $ oLongWithoutGroup || oNoGroup
    author = oAuthor
    context = oContext
    icon = oIcon
    colorize = case oColor of
      _ | oNoneSortExtra -> Nothing
      W.NEVER -> Nothing
      W.ALWAYS -> Just oExtraColor
      W.AUTO -> if toTTY then Just oExtraColor else Nothing
    hyperlink = case oHyperlink of
      W.NEVER -> False
      W.ALWAYS -> True
      W.AUTO -> toTTY
    indicatorStyle =
      let classify = case oClassify of
            W.NEVER -> IndicatorNone
            W.ALWAYS -> IndicatorClassify
            W.AUTO ->
              if toTTY
                then IndicatorClassify
                else IndicatorNone
          dir =
            if oDirectoryIndicator
              then IndicatorSlash
              else IndicatorNone
          fileType =
            if oFileType
              then IndicatorFiletype
              else IndicatorNone
          style = oIndicatorStyle
       in maximum [classify, dir, fileType, style]
    listing
      | oAll || oNoneSortExtra = All
      | oAlmostAll = AlmostAll
      | otherwise = NoHidden
    -- Probably we should throw a runtime error or output an error message if
    -- fail to parse the block size come from the environment variable.
    fileSize = case oBlockSize of
      DefaultSize
        | oHumanReadable -> HumanReadable
        | otherwise -> fromMaybe DefaultSize $ Env.blockSize env >>= Size.parseBlockSize
      blocksize -> blocksize
    blockSize = case oBlockSize of
      DefaultSize
        | oHumanReadable -> HumanReadable
        | oKibibyte -> DefaultSize
        | otherwise -> fromMaybe DefaultSize $ Env.blockSize env >>= Size.parseBlockSize
      blocksize -> blocksize
    ignoreBackups = oIgnoreBackups
    format
      | oVertical = VERTICAL
      | oHorihontal = HORIZONTAL
      | oFillWidth = COMMAS
      | oOneline = SINGLECOLUMN
      | or [oLong, oLongWithoutGroup, oLongWithoutOwner, oFullTime] = LONG
      | Just fmt <- oFormat = fmt
      | toTTY && not zero = VERTICAL
      | otherwise = SINGLECOLUMN
    directory = oDirectory
    groupDirectoriesFirst = oGroupDirectoriesFirst
    si = oSi
    dereferenceCommandLine = oDereferenceCommandLine
    dereferenceCommandLineSymlinkToDir = oDereferenceCommandLineSymlinkToDir
    hide = compile <$> oHide
    ignore = compile <$> oIgnore
    level = oLevel
    dereference = oDereference
    numericUidGid = oNumericUidGid
    quotingStyle
      | oLiteral = Literal
      | oQuoteName = C
      | oEscape = Escape
      | Just style <- (Env.quotingStyle env >>= Quote.parseQuotingStyle . T.pack) <|> oQuotingStyle = style
      | toTTY = ShellEscape
      | otherwise = Literal
    showControlChars = fromMaybe (not toTTY) oShowControlChars
    reverse = oReverse
    recursive = oRecursive
    sort = case oSort of
      _ | oNoneSortExtra -> NONE
      NAME
        | oNoneSort -> NONE
        | oSizeSort -> SIZE
        | oTimeSort || (oCtime || oAtime) && format /= LONG -> TIME
        | oNaturalSort -> VERSION
        | oExtensionSort -> EXTENSION
        | otherwise -> NAME
      x -> x
    time
      | oCtime = CHANGE
      | oAtime = ACCESS
      | otherwise = oTime
    timeStyle
      | oFullTime = Just FULLISO
      | otherwise = oTimeStyle
    tabSize = if oTabSeparator then Just oTabSize else Nothing
    tree = oTree
    width = case format of
      Format.SINGLECOLUMN -> 1
      Format.LONG -> 1
      _ -> fromMaybe 80 $ oWidth <|> Env.columnSize env
    zero = oZero
    dired = oDired && format == LONG && not tree
    toTTY = Env.toTerminal env
    currentWorkingDirectory = Env.cwd env
    hostname = T.pack $ Env.hostname env
