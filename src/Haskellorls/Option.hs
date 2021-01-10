module Haskellorls.Option
  ( Option (..)
  , ColorOpt(..)
  , opts
  ) where

import Control.Applicative (many)
import qualified Options.Applicative as OA

data Option = Option
  { color :: ColorOpt
  , long :: Bool
  , humanReadable :: Bool
  , blockSize :: String
  , targets :: [String]
  } deriving Show

data ColorOpt = NEVER | ALWAYS | AUTO
  deriving Show

opts :: OA.ParserInfo Option
opts = OA.info (optionParser OA.<**> OA.helper)
  (  OA.fullDesc
  <> OA.progDesc "Haskellorls = Haskell color ls"
  <> OA.header "HEADER"
  )

optionParser :: OA.Parser Option
optionParser = Option
  <$> colorParser
  <*> longParser
  <*> humanReadableParser
  <*> blockSizeParser
  <*> argParser

colorParser :: OA.Parser ColorOpt
colorParser = OA.option parseColorOpt
  ( OA.long "color"
    <> OA.metavar "WHEN"
    <> OA.value NEVER
  )

parseColorOpt :: OA.ReadM ColorOpt
parseColorOpt = OA.str >>= f
  where f s = case s of
                "never" -> return NEVER
                "always" -> return ALWAYS
                "auto" -> return AUTO
                _ -> OA.readerError "Only never, always or auto"

longParser :: OA.Parser Bool
longParser = OA.switch (OA.short 'l')

humanReadableParser :: OA.Parser Bool
humanReadableParser = OA.switch
  ( OA.short 'h'
    <> OA.long "human-readable"
  )

blockSizeParser :: OA.Parser String
blockSizeParser = OA.strOption
  ( OA.long "block-size"
    <> OA.metavar "SIZE"
    <> OA.value ""
  )

argParser :: OA.Parser [String]
argParser = many . OA.strArgument $ OA.metavar "[FILE]..."
