module Haskellorls.Config
  ( Config (..),
    mkConfig,
    isLongStyle,
  )
where

import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import qualified Haskellorls.Config.Depth as Depth
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
    colorize :: Bool,
    extraColor :: Bool,
    hyperlink :: Bool,
    indicatorStyle :: IndicatorStyle,
    listing :: ListingStyle,
    fileSize :: Size.BlockSize,
    blockSize :: Size.BlockSize,
    ignoreBackups :: Bool,
    directory :: Bool,
    format :: Format.Format,
    groupDirectoriesFirst :: Bool,
    si :: Bool,
    dereferenceCommandLine :: Bool,
    dereferenceCommandLineSymlinkToDir :: Bool,
    hide :: String,
    ignore :: String,
    level :: Depth.Depth,
    dereference :: Bool,
    numericUidGid :: Bool,
    quoteName :: Bool,
    quotingStyle :: Quote.QuotingStyle,
    reverse :: Bool,
    recursive :: Bool,
    sort :: Sort.SortType,
    time :: Time.TimeType,
    timeStyle :: Time.TimeStyle,
    tabSeparator :: Bool,
    tabSize :: Int,
    tree :: Bool,
    width :: Int,
    noQuote :: Bool,
    zero :: Bool,
    dired :: Bool,
    toTTY :: Bool,
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
      _ | oNoneSortExtra -> False
      W.NEVER -> False
      W.ALWAYS -> True
      W.AUTO -> toTTY
    extraColor = oExtraColor
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
      | otherwise =
          case oFormat of
            Just fmt -> fmt
            Nothing
              | toTTY && not zero -> VERTICAL
              | otherwise -> SINGLECOLUMN
    directory = oDirectory
    groupDirectoriesFirst = oGroupDirectoriesFirst
    si = oSi
    dereferenceCommandLine = oDereferenceCommandLine
    dereferenceCommandLineSymlinkToDir = oDereferenceCommandLineSymlinkToDir
    hide = oHide
    ignore = oIgnore
    level = oLevel
    dereference = oDereference
    numericUidGid = oNumericUidGid
    quoteName = oQuoteName
    -- Probably we should throw a runtime error or output an error message if
    -- fail to parse the block size come from the environment variable.
    quotingStyle = case fromMaybe oQuotingStyle $ Env.quotingStyle env >>= Quote.parseQuotingStyle . T.pack of
      style
        | oLiteral -> Literal
        | oEscape -> Escape
        | oHideControlChars && not oShowControlChars -> Literal
        | otherwise -> style
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
      | oFullTime = FULLISO
      | otherwise = oTimeStyle
    tabSeparator = oTabSeparator
    tabSize = oTabSize
    tree = oTree
    width = case format of
      Format.SINGLECOLUMN -> 1
      Format.LONG -> 1
      _ -> fromMaybe 80 $ oWidth <|> Env.columnSize env
    noQuote = False
    zero = oZero
    dired = oDired && format == LONG && not tree
    toTTY = Env.toTerminal env
    currentWorkingDirectory = Env.cwd env
    hostname = T.pack $ Env.hostname env

isLongStyle :: Config -> Bool
isLongStyle config = case format config of
  LONG -> True
  _ -> False
