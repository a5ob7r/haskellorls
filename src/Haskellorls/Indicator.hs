module Haskellorls.Indicator
  ( buildIndicatorPrinter,
    deriveIndicatorStyle,
  )
where

import qualified Haskellorls.Name as Name
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.YetAnotherString as YAString

data Indicators = Indicators
  { indicatorsDirectory :: String,
    indicatorsLink :: String,
    indicatorsPipe :: String,
    indicatorsSocket :: String,
    indicatorsDoor :: String,
    indicatorsExecutable :: String
  }

noneIndicators :: Indicators
noneIndicators =
  Indicators
    { indicatorsDirectory = "",
      indicatorsLink = "",
      indicatorsPipe = "",
      indicatorsSocket = "",
      indicatorsDoor = "",
      indicatorsExecutable = ""
    }

slashIndicator :: Indicators
slashIndicator = noneIndicators {indicatorsDirectory = directoryIndicator}

fileTypeIndicator :: Indicators
fileTypeIndicator =
  noneIndicators
    { indicatorsDirectory = directoryIndicator,
      indicatorsLink = linkIndicator,
      indicatorsPipe = pipeIndicator,
      indicatorsSocket = socketIndicator,
      indicatorsDoor = doorIndicator
    }

classifyIndicators :: Indicators
classifyIndicators =
  fileTypeIndicator
    { indicatorsDirectory = directoryIndicator,
      indicatorsExecutable = executableIndicator
    }

indicatorSelector :: Node.NodeInfo -> Indicators -> String
indicatorSelector node = case Name.nodeTypeOf (Node.nodeInfoStatus node) of
  Name.Directory -> indicatorsDirectory
  Name.SymbolicLink -> indicatorsLink
  Name.NamedPipe -> indicatorsPipe
  Name.Socket -> indicatorsSocket
  Name.DoorsDevise -> indicatorsDoor
  Name.Executable -> indicatorsExecutable
  _ -> const ""

buildIndicatorPrinter :: Option.Option -> Node.NodeInfo -> [YAString.WrapedString]
buildIndicatorPrinter opt node = if null indicator then [] else YAString.toWrappedStringArray indicator
  where
    indicator = indicatorSelector node $ buildIndicators opt

buildIndicators :: Option.Option -> Indicators
buildIndicators opt =
  if long && not oneline
    then indicators {indicatorsLink = ""}
    else indicators
  where
    indicators = case deriveIndicatorStyle opt of
      Option.IndicatorNone -> noneIndicators
      Option.IndicatorFiletype -> fileTypeIndicator
      Option.IndicatorSlash -> slashIndicator
      Option.IndicatorClassify -> classifyIndicators
    oneline = Option.oneline opt
    long = any (\f -> f opt) [Option.long, Option.longWithoutOwner, Option.longWithoutGroup]

deriveIndicatorStyle :: Option.Option -> Option.IndicatorStyle
deriveIndicatorStyle opt = maximum [classify, directory, fileType, style]
  where
    classify =
      if Option.classify opt
        then Option.IndicatorClassify
        else Option.IndicatorNone
    directory =
      if Option.directoryIndicator opt
        then Option.IndicatorSlash
        else Option.IndicatorNone
    fileType =
      if Option.fileType opt
        then Option.IndicatorFiletype
        else Option.IndicatorNone
    style = Option.indicatorStyle opt

directoryIndicator :: String
directoryIndicator = "/"

linkIndicator :: String
linkIndicator = "@"

pipeIndicator :: String
pipeIndicator = "|"

socketIndicator :: String
socketIndicator = "="

doorIndicator :: String
doorIndicator = ">"

executableIndicator :: String
executableIndicator = "*"
