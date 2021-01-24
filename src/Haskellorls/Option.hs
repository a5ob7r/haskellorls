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
    targets :: [String]
  }
  deriving (Show)

data ColorOpt = NEVER | ALWAYS | AUTO
  deriving (Show)

opts :: OA.ParserInfo Option
opts =
  OA.info
    (optionParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Haskellorls = Haskell color ls"
        <> OA.header "HEADER"
    )

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
    <*> argParser

colorParser :: OA.Parser ColorOpt
colorParser =
  OA.option
    parseColorOpt
    ( OA.long "color"
        <> OA.metavar "WHEN"
        <> OA.value NEVER
    )

parseColorOpt :: OA.ReadM ColorOpt
parseColorOpt = OA.str >>= f
  where
    f s = case s of
      "never" -> return NEVER
      "always" -> return ALWAYS
      "auto" -> return AUTO
      _ -> OA.readerError "Only never, always or auto"

longParser :: OA.Parser Bool
longParser = OA.switch (OA.short 'l')

humanReadableParser :: OA.Parser Bool
humanReadableParser =
  OA.switch
    ( OA.short 'h'
        <> OA.long "human-readable"
    )

blockSizeParser :: OA.Parser String
blockSizeParser =
  OA.strOption
    ( OA.long "block-size"
        <> OA.metavar "SIZE"
        <> OA.value ""
    )

timeParser :: OA.Parser String
timeParser =
  OA.strOption
    ( OA.long "time"
        <> OA.metavar "WORD"
        <> OA.value ""
    )

timeStyleParser :: OA.Parser String
timeStyleParser =
  OA.strOption
    ( OA.long "time-style"
        <> OA.metavar "TYPE_STYLE"
        <> OA.value ""
    )

allParser :: OA.Parser Bool
allParser =
  OA.switch
    ( OA.short 'a'
        <> OA.long "all"
    )

almostAllParser :: OA.Parser Bool
almostAllParser =
  OA.switch
    ( OA.short 'A'
        <> OA.long "almost-all"
    )

sortParser :: OA.Parser String
sortParser =
  OA.strOption
    ( OA.long "sort"
        <> OA.metavar "WORD"
        <> OA.value "name"
    )

reverseParser :: OA.Parser Bool
reverseParser =
  OA.switch
    ( OA.short 'r'
        <> OA.long "reverse"
    )

argParser :: OA.Parser [String]
argParser = many . OA.strArgument $ OA.metavar "[FILE]..."
