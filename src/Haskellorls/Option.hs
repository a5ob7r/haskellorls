module Haskellorls.Option
  ( Option (..),
    ColorOpt (..),
    IndicatorStyle (..),
    opts,
  )
where

import qualified Control.Applicative as A
import qualified Options.Applicative as OA

data Option = Option
  { color :: ColorOpt,
    extraColor :: Bool,
    long :: Bool,
    humanReadable :: Bool,
    blockSize :: String,
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
    indicatorStyle :: IndicatorStyle,
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
    <*> humanReadableParser
    <*> blockSizeParser
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
    <*> indicatorStyleParser
    <*> versionParser
    <*> argParser

colorParser :: OA.Parser ColorOpt
colorParser =
  OA.option parseColorOpt $
    OA.long "color"
      <> OA.metavar "WHEN"
      <> OA.value NEVER

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
longParser = OA.switch $ OA.short 'l'

humanReadableParser :: OA.Parser Bool
humanReadableParser =
  OA.switch $
    OA.short 'h'
      <> OA.long "human-readable"

blockSizeParser :: OA.Parser String
blockSizeParser =
  OA.strOption $
    OA.long "block-size"
      <> OA.metavar "SIZE"
      <> OA.value ""

timeParser :: OA.Parser String
timeParser =
  OA.strOption $
    OA.long "time"
      <> OA.metavar "WORD"
      <> OA.value ""

timeStyleParser :: OA.Parser String
timeStyleParser =
  OA.strOption $
    OA.long "time-style"
      <> OA.metavar "TYPE_STYLE"
      <> OA.value ""

allParser :: OA.Parser Bool
allParser =
  OA.switch $
    OA.short 'a'
      <> OA.long "all"

almostAllParser :: OA.Parser Bool
almostAllParser =
  OA.switch $
    OA.short 'A'
      <> OA.long "almost-all"

sortParser :: OA.Parser String
sortParser =
  OA.strOption $
    OA.long "sort"
      <> OA.metavar "WORD"
      <> OA.value "name"

reverseParser :: OA.Parser Bool
reverseParser =
  OA.switch $
    OA.short 'r'
      <> OA.long "reverse"

onelineParser :: OA.Parser Bool
onelineParser = OA.switch $ OA.short '1'

noGroupParser :: OA.Parser Bool
noGroupParser =
  OA.switch $
    OA.short 'G'
      <> OA.long "no-group"

longWithoutGroupParser :: OA.Parser Bool
longWithoutGroupParser = OA.switch $ OA.short 'o'

longWithoutOwnerParser :: OA.Parser Bool
longWithoutOwnerParser = OA.switch $ OA.short 'g'

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

versionParser :: OA.Parser Bool
versionParser =
  OA.switch $
    OA.long "version"
      <> OA.help "Show version info"

argParser :: OA.Parser [String]
argParser = A.many . OA.strArgument $ OA.metavar "[FILE]..."
