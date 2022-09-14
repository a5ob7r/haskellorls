module Haskellorls.Config
  ( Config (..),
    mkConfig,
    disableDereferenceOnCommandLine,
  )
where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as C
import Data.Either.Extra (eitherToMaybe)
import Data.Gettext (gettext)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Haskellorls.Config.Environment as Env
import Haskellorls.Config.Format (Format (..))
import Haskellorls.Config.Indicator (IndicatorStyle (..))
import Haskellorls.Config.Listing (ListingStyle (..))
import Haskellorls.Config.Option (Option (..))
import Haskellorls.Config.Quote (QuotingStyle (..))
import Haskellorls.Config.Size (BlockSize (..), BlockSizeMod (..))
import Haskellorls.Config.Sort (SortType (..))
import Haskellorls.Config.TimeStyle (TimeStyle (..))
import Haskellorls.Config.TimeType (TimeType (..))
import qualified Haskellorls.Config.When as W
import Haskellorls.Data.Infinitable (Infinitable)
import System.FilePath.Glob (Pattern, compile)
import System.FilePath.Posix.ByteString (RawFilePath)
import Witch (from, into, tryFrom)
import Prelude hiding (lookup, reverse)

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
    fileSize :: BlockSizeMod BlockSize,
    blockSize :: BlockSizeMod BlockSize,
    ignoreBackups :: Bool,
    directory :: Bool,
    -- | When this value is 'COMMAS', every column alignment in format is
    -- disabled.
    format :: Format,
    groupDirectoriesFirst :: Bool,
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
    quotingStyle :: QuotingStyle,
    -- | Whether or not to show any control (or non-printable) charaster as is,
    -- or to replace them by @?@, when the 'quotingStyle' is 'Literal'. By
    -- default this value is 'True' unless the stdout is connected to a
    -- terminal, otherwise 'False'.
    showControlChars :: Bool,
    reverse :: Bool,
    recursive :: Bool,
    sort :: SortType,
    time :: TimeType,
    timeStyle :: TimeStyle,
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
      let classify = case fromMaybe W.NEVER oClassify of
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
    fileSize = case oBlockSize of
      Just blocksize -> blocksize
      Nothing
        | oHumanReadable -> NoMod HumanReadableBI
        | oSi -> NoMod HumanReadableSI
        | Just filesize <- Env.blockSize env -> fromMaybe (NoMod . BlockSize . maybe 1024 (const 512) $ Env.posixlyCorrect env) . eitherToMaybe $ tryFrom filesize
        | otherwise -> NoMod $ BlockSize 1
    blockSize = case oBlockSize of
      Just blocksize -> blocksize
      Nothing
        | oHumanReadable -> NoMod HumanReadableBI
        | oSi -> NoMod HumanReadableSI
        | oKibibyte -> NoMod $ BlockSize 1024
        | Just blocksize <- Env.blockSize env <|> Env.onlyBlockSize env >>= eitherToMaybe . tryFrom -> blocksize
        | otherwise -> NoMod . BlockSize . maybe 1024 (const 512) $ Env.posixlyCorrect env
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
    dereferenceCommandLine = oDereferenceCommandLine
    dereferenceCommandLineSymlinkToDir
      | oDereferenceCommandLineSymlinkToDir = True
      | LONG <- format = False
      | Just _ <- oClassify = False
      | directory || dereference || dereferenceCommandLine = False
      | otherwise = True
    hide = compile <$> oHide
    ignore = compile <$> oIgnore
    level = oLevel
    dereference = oDereference
    numericUidGid = oNumericUidGid
    quotingStyle
      | oLiteral = Literal
      | oQuoteName = C
      | oEscape = Escape
      | Just style <- oQuotingStyle <|> (Env.quotingStyle env >>= eitherToMaybe . tryFrom) =
          let lookup l r catalog = do
                (lh, lt) <- TL.uncons $ gettext catalog (C.singleton l)
                (rh, rt) <- TL.uncons $ gettext catalog (C.singleton r)
                if TL.null lt && TL.null rt then Just (lh, rh) else Nothing
           in case style of
                CLocale l r
                  | Just catalog <- Env.catalog env, Just (lh, rh) <- lookup l r catalog -> CLocale lh rh
                  | maybe False ("UTF-8" `isSuffixOf`) . from $ Env.lcMessages env -> CLocale '‘' '’'
                  | otherwise -> CLocale '"' '"'
                Locale l r
                  | Just catalog <- Env.catalog env, Just (lh, rh) <- lookup l r catalog -> Locale lh rh
                  | maybe False ("UTF-8" `isSuffixOf`) . from $ Env.lcMessages env -> Locale '‘' '’'
                  | maybe False (`elem` ["C", "POSIX"]) . into @(Maybe String) $ Env.lcMessages env -> Locale '\'' '\''
                _ -> style
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
      | oFullTime = FULLISO
      | otherwise = fromMaybe LOCALE $ oTimeStyle <|> (Env.timeStyle env >>= eitherToMaybe . tryFrom)
    tabSize = if oTabSeparator then Just oTabSize else Nothing
    tree = oTree
    width = case format of
      SINGLECOLUMN -> 1
      LONG -> 1
      _ -> fromMaybe 80 $ oWidth <|> Env.columnSize env
    zero = oZero
    dired = oDired && format == LONG && not tree
    toTTY = Env.toTerminal env
    currentWorkingDirectory = Env.cwd env
    hostname = T.pack $ Env.hostname env

-- | Disable 'dereferenceCommandLine' and 'dereferenceCommandLineSymlinkToDir'
-- of 'Config'.
disableDereferenceOnCommandLine :: Config -> Config
disableDereferenceOnCommandLine config = config {dereferenceCommandLine = False, dereferenceCommandLineSymlinkToDir = False}
