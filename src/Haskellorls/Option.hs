module Haskellorls.Option
  ( Option (..),
    ColorOpt (..),
    opts,
  )
where

import Control.Applicative (many)
import qualified Options.Applicative as OA

data Option = Option
  { color :: ColorOpt,
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
    targets :: [FilePath]
  }
  deriving (Show)

data ColorOpt = NEVER | ALWAYS | AUTO
  deriving (Show)

opts :: OA.ParserInfo Option
opts =
  OA.info (optionParser OA.<**> OA.helper) $
    OA.fullDesc
      <> OA.progDesc "Haskellorls = Haskell color ls"

optionParser :: OA.Parser Option
optionParser =
  Option
    <$> colorParser
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

argParser :: OA.Parser [String]
argParser = many . OA.strArgument $ OA.metavar "[FILE]..."
