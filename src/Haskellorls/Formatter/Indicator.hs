module Haskellorls.Formatter.Indicator (buildIndicatorPrinter) where

import qualified Data.Text as T
import qualified Haskellorls.Config as Config
import Haskellorls.Config.Indicator
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.NodeInfo as Node

data Indicators = Indicators
  { indicatorsDirectory :: T.Text,
    indicatorsLink :: T.Text,
    indicatorsPipe :: T.Text,
    indicatorsSocket :: T.Text,
    indicatorsDoor :: T.Text,
    indicatorsExecutable :: T.Text
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

indicatorSelector :: Node.NodeInfo -> Indicators -> T.Text
indicatorSelector node = case Node.nodeType node of
  Node.Directory -> indicatorsDirectory
  Node.Sticky -> indicatorsDirectory
  Node.OtherWritable -> indicatorsDirectory
  Node.StickyOtherWritable -> indicatorsDirectory
  Node.SymbolicLink -> indicatorsLink
  Node.NamedPipe -> indicatorsPipe
  Node.Socket -> indicatorsSocket
  Node.DoorsDevise -> indicatorsDoor
  Node.Executable -> indicatorsExecutable
  _ -> const ""

buildIndicatorPrinter :: Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
buildIndicatorPrinter config node = [Attr.Other $ WT.deserialize indicator | not (T.null indicator)]
  where
    indicator = indicatorSelector node' $ buildIndicators config
    node' = case Node.getNodeLinkInfo node of
      Just (Right _) | Config.isLongStyle config -> Node.toFileInfo node
      _ -> node

buildIndicators :: Config.Config -> Indicators
buildIndicators config =
  if Config.isLongStyle config
    then indicators {indicatorsLink = ""}
    else indicators
  where
    indicators = case Config.indicatorStyle config of
      IndicatorNone -> noneIndicators
      IndicatorFiletype -> fileTypeIndicator
      IndicatorSlash -> slashIndicator
      IndicatorClassify -> classifyIndicators

directoryIndicator :: T.Text
directoryIndicator = "/"

linkIndicator :: T.Text
linkIndicator = "@"

pipeIndicator :: T.Text
pipeIndicator = "|"

socketIndicator :: T.Text
socketIndicator = "="

doorIndicator :: T.Text
doorIndicator = ">"

executableIndicator :: T.Text
executableIndicator = "*"
