{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.Indicator.Decorator
  ( buildIndicatorPrinter,
    deriveIndicatorStyle,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Format.Util as Format
import Haskellorls.Indicator.Type
import qualified Haskellorls.Name.Decorator as Name
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.WrappedText as WT

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
indicatorSelector node = case Name.nodeTypeOf (Node.nodeInfoStatus node) of
  Name.Directory -> indicatorsDirectory
  Name.Sticky -> indicatorsDirectory
  Name.OtherWritable -> indicatorsDirectory
  Name.StickyOtherWritable -> indicatorsDirectory
  Name.SymbolicLink -> indicatorsLink
  Name.NamedPipe -> indicatorsPipe
  Name.Socket -> indicatorsSocket
  Name.DoorsDevise -> indicatorsDoor
  Name.Executable -> indicatorsExecutable
  _ -> const ""

buildIndicatorPrinter :: Option.Option -> Node.NodeInfo -> [WT.WrappedText]
buildIndicatorPrinter opt node = if T.null indicator then [] else WT.toWrappedTextSingleton indicator
  where
    indicator = indicatorSelector node' $ buildIndicators opt
    node' = case node of
      Node.LinkInfo {} | Format.isLongStyle opt -> Node.toFileInfo node
      _ -> node

buildIndicators :: Option.Option -> Indicators
buildIndicators opt =
  if Format.isLongStyle opt
    then indicators {indicatorsLink = ""}
    else indicators
  where
    indicators = case deriveIndicatorStyle opt of
      IndicatorNone -> noneIndicators
      IndicatorFiletype -> fileTypeIndicator
      IndicatorSlash -> slashIndicator
      IndicatorClassify -> classifyIndicators

deriveIndicatorStyle :: Option.Option -> IndicatorStyle
deriveIndicatorStyle opt = maximum [classify, directory, fileType, style]
  where
    classify =
      if Option.classify opt
        then IndicatorClassify
        else IndicatorNone
    directory =
      if Option.directoryIndicator opt
        then IndicatorSlash
        else IndicatorNone
    fileType =
      if Option.fileType opt
        then IndicatorFiletype
        else IndicatorNone
    style = Option.indicatorStyle opt

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
