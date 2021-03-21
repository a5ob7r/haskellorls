{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.Decorator
  ( Printer,
    Printers (..),
    PrinterType (..),
    buildPrinters,
    buildLines,
    buildPrinterTypes,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Clock
import qualified Data.Time.Format as TFormat
import qualified Data.Time.LocalTime as LClock
import qualified Haskellorls.Color.Utils as Color
import qualified Haskellorls.Context as Context
import qualified Haskellorls.Field as Field
import qualified Haskellorls.Format.Util as Format
import qualified Haskellorls.Icon as Icon
import qualified Haskellorls.Indicator.Decorator as Indicator
import qualified Haskellorls.Indicator.Type as Indicator
import qualified Haskellorls.Inode as Inode
import qualified Haskellorls.Link as Link
import qualified Haskellorls.LsColor.Config as Color
import qualified Haskellorls.Name.Decorator as Name
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Ownership.Decorator as Ownership
import qualified Haskellorls.Size.Decorator as Size
import qualified Haskellorls.SymbolicLink as SymbolicLink
import qualified Haskellorls.Time.Decorator as Time
import qualified Haskellorls.Tree.Decorator as Tree
import qualified Haskellorls.WrappedText as WT

data PrinterType
  = FILEINODE
  | FILEBLOCK
  | FILEFIELD
  | FILELINK
  | FILEOWNER
  | FILEGROUP
  | FILEAUTHOR -- For compatibility to GNU ls
  | FILECONTEXT
  | FILESIZE
  | FILETIME
  | FILENAME
  | FILENAMEWITHDQUOTE

data NamePrinterType
  = TREE
  | ICON
  | NAME
  | LINK
  | INDICATOR

data NodeNamePrinters = NodeNamePrinters
  { nodeTreePrinter :: Printer,
    nodeIconPrinter :: Printer,
    nodeNamePrinter :: Printer,
    nodeLinkPrinter :: Printer,
    nodeIndicatorPrinter :: Printer
  }

nodeNamePrinterSelector :: NamePrinterType -> NodeNamePrinters -> Printer
nodeNamePrinterSelector npType = selector
  where
    selector = case npType of
      TREE -> nodeTreePrinter
      ICON -> nodeIconPrinter
      NAME -> nodeNamePrinter
      LINK -> nodeLinkPrinter
      INDICATOR -> nodeIndicatorPrinter

neededNamePrinterTypeBy :: NamePrinterType -> Option.Option -> Bool
neededNamePrinterTypeBy npType opt = case npType of
  TREE -> Option.tree opt
  ICON -> Option.icon opt
  NAME -> True
  LINK -> Format.isLongStyle opt
  INDICATOR -> Indicator.IndicatorNone < Indicator.deriveIndicatorStyle opt

buildNamePrinterTypes :: Option.Option -> [NamePrinterType]
buildNamePrinterTypes opt = filter (`neededNamePrinterTypeBy` opt) [TREE, ICON, NAME, LINK, INDICATOR]

buildNodeNamePrinter :: Option.Option -> NodeNamePrinters -> Printer
buildNodeNamePrinter opt printers node = concatMap (\npType -> nodeNamePrinterSelector npType printers node) $ buildNamePrinterTypes opt

type Printer = Node.NodeInfo -> [WT.WrappedText]

type Alignmenter = [WT.WrappedText] -> [WT.WrappedText]

type AlignmenterBuilder = T.Text -> Int -> Alignmenter

data AlighmentType
  = NONE
  | LEFT
  | RIGHT

data Printers = Printers
  { fileInodePrinter :: Printer,
    fileBlockPrinter :: Printer,
    fileFieldPrinter :: Printer,
    fileLinkPrinter :: Printer,
    fileOwnerPrinter :: Printer,
    fileGroupPrinter :: Printer,
    fileContextPrinter :: Printer,
    fileSizePrinter :: Printer,
    fileTimePrinter :: Printer,
    fileNamePrinter :: Printer,
    fileNameWithDQuotePrinter :: Printer
  }

alignmentTypeFor :: PrinterType -> AlighmentType
alignmentTypeFor dType = case dType of
  FILEINODE -> RIGHT
  FILEBLOCK -> RIGHT
  FILEFIELD -> NONE
  FILELINK -> RIGHT
  FILEOWNER -> LEFT
  FILEGROUP -> LEFT
  FILEAUTHOR -> LEFT
  FILECONTEXT -> LEFT
  FILESIZE -> RIGHT
  FILETIME -> LEFT
  FILENAME -> NONE
  FILENAMEWITHDQUOTE -> NONE

printerSelectorFor :: PrinterType -> Printers -> Printer
printerSelectorFor pType = case pType of
  FILEINODE -> fileInodePrinter
  FILEBLOCK -> fileBlockPrinter
  FILEFIELD -> fileFieldPrinter
  FILELINK -> fileLinkPrinter
  FILEOWNER -> fileOwnerPrinter
  FILEGROUP -> fileGroupPrinter
  FILEAUTHOR -> fileOwnerPrinter
  FILECONTEXT -> fileContextPrinter
  FILESIZE -> fileSizePrinter
  FILETIME -> fileTimePrinter
  FILENAME -> fileNamePrinter
  FILENAMEWITHDQUOTE -> fileNameWithDQuotePrinter

alignmenterBuilderSelectorFor :: AlighmentType -> AlignmenterBuilder
alignmenterBuilderSelectorFor aType = case aType of
  NONE -> noPadding
  LEFT -> rightPadding
  RIGHT -> leftPadding

buildPrinters :: Option.Option -> IO Printers
buildPrinters opt = do
  cConfig <- Color.config
  uidSubstTable <- if Option.numericUidGid opt then return Map.empty else Ownership.getUserIdSubstTable
  gidSubstTable <- if Option.numericUidGid opt then return Map.empty else Ownership.getGroupIdSubstTable
  userInfo <- Ownership.userInfo
  currentTime <- Clock.getCurrentTime
  timeZone <- LClock.getCurrentTimeZone

  let shouldColorize = Color.shouldColorize opt
      isEnableExtraColor = Option.extraColor opt

      fileInodePrinter =
        if shouldColorize && isEnableExtraColor
          then Inode.nodeInodeNumberWithColor cConfig
          else WT.toWrappedTextSingleton . T.pack . show . Inode.nodeInodeNumber

      fileBlockPrinter =
        if shouldColorize && isEnableExtraColor
          then Size.coloredFileBlockSize cConfig opt
          else Size.fileBlockSize opt

      filemodeFieldPrinter =
        if shouldColorize && isEnableExtraColor
          then Field.showFilemodeFieldWithColor cConfig
          else Field.showFilemodeField

      fileLinkPrinter =
        if shouldColorize && isEnableExtraColor
          then Link.nodeLinksNumberWithColor cConfig
          else WT.toWrappedTextSingleton . T.pack . show . Link.nodeLinksNumber

      fileOwnerPrinter =
        if shouldColorize && isEnableExtraColor
          then Ownership.coloredOwnerName uidSubstTable cConfig userInfo
          else WT.toWrappedTextSingleton . Ownership.ownerName uidSubstTable

      fileGroupPrinter =
        if shouldColorize && isEnableExtraColor
          then Ownership.coloredGroupName gidSubstTable cConfig userInfo
          else WT.toWrappedTextSingleton . Ownership.groupName gidSubstTable

      fileContextPrinter =
        if shouldColorize && isEnableExtraColor
          then Context.colorizedContext cConfig
          else WT.toWrappedTextSingleton . Context.context

      fileSizePrinter =
        if shouldColorize && isEnableExtraColor
          then Size.coloredFileSize cConfig opt
          else Size.fileSize opt

      fileTimePrinter =
        if shouldColorize && isEnableExtraColor
          then Time.coloredTimeStyleFunc cConfig timeZone TFormat.defaultTimeLocale currentTime timeStyle . fileTime
          else WT.toWrappedTextSingleton . timeStyleFunc . fileTime
        where
          timeStyleFunc = Time.timeStyleFunc timeZone TFormat.defaultTimeLocale currentTime timeStyle
          fileTime = Clock.posixSecondsToUTCTime . Time.fileTime (Time.timeType opt) . Node.getNodeStatus
          timeStyle = Time.timeStyle opt

      -- TODO: Should use colored icon? But, must consider charactor size and background color.
      -- e.g. $ echo -e '\e[48;5;196;38;5;232;1m\e[0m'
      nodeTreePrinter =
        if shouldColorize
          then Tree.treeBranchWithColor cConfig . Node.getTreeNodePositions
          else WT.toWrappedTextSingleton . Tree.treeBranch . Node.getTreeNodePositions
      nodeIconPrinter = flip Icon.lookupIcon Icon.defaultConfig
      buildNamePrinter option =
        if shouldColorize
          then Name.colorizedNodeNameWrapper option cConfig
          else Name.nodeNameWrapper option
      nodeNamePrinter = buildNamePrinter opt {Option.noQuote = True}
      nodeNameWithDQuotePrinter = buildNamePrinter opt
      nodeIndicatorPrinter = Indicator.buildIndicatorPrinter opt
      nodeLinkPrinter =
        if shouldColorize
          then SymbolicLink.coloredLinkName opt cConfig
          else SymbolicLink.linkName opt
      nodeNamePrinters = NodeNamePrinters {..}
      nodeNameWithDQuotePrinters = NodeNamePrinters {nodeNamePrinter = nodeNameWithDQuotePrinter, ..}

  return $
    Printers
      { fileFieldPrinter = filemodeFieldPrinter . Field.filemodeField . Node.getNodeStatus,
        fileNamePrinter = buildNodeNamePrinter opt nodeNamePrinters,
        fileNameWithDQuotePrinter = buildNodeNamePrinter opt nodeNameWithDQuotePrinters,
        ..
      }

buildPrinterTypes :: Option.Option -> [PrinterType]
buildPrinterTypes opt = filter (`neededBy` opt) [FILEINODE, FILEBLOCK, FILEFIELD, FILELINK, FILEOWNER, FILEGROUP, FILEAUTHOR, FILECONTEXT, FILESIZE, FILETIME, FILENAME, FILENAMEWITHDQUOTE]

-- | Should the `PrinterType` value is needed by the options.
neededBy :: PrinterType -> Option.Option -> Bool
neededBy pType opt = case pType of
  FILEINODE -> inode
  FILEBLOCK -> size
  FILEFIELD -> long
  FILELINK -> long
  FILEOWNER -> long && owner
  FILEGROUP -> long && group
  FILEAUTHOR -> long && author
  FILECONTEXT -> context
  FILESIZE -> long
  FILETIME -> long
  FILENAME -> noQuote
  FILENAMEWITHDQUOTE -> not noQuote
  where
    inode = Option.inode opt
    size = Option.size opt
    long = Format.isLongStyle opt
    owner = not $ Option.longWithoutOwner opt
    group = not (Option.longWithoutGroup opt || Option.noGroup opt)
    author = Option.author opt
    context = Option.context opt
    noQuote = Option.noQuote opt

buildColumn :: [Node.NodeInfo] -> Printers -> PrinterType -> [[WT.WrappedText]]
buildColumn nodes printers pType = map alignmenter nodes'
  where
    printer = printerSelectorFor pType printers
    nodes' = map printer nodes
    maxLen = maximum $ map (sum . map WT.wtLengthForDisplay) nodes'
    aType = alignmentTypeFor pType
    aBuilder = alignmenterBuilderSelectorFor aType
    alignmenter = aBuilder " " maxLen

buildGrid :: [Node.NodeInfo] -> Printers -> [PrinterType] -> [[[WT.WrappedText]]]
buildGrid nodes printers = List.transpose . map (buildColumn nodes printers)

buildLines :: [Node.NodeInfo] -> Printers -> [PrinterType] -> [[WT.WrappedText]]
buildLines nodes printers types = map cat $ buildGrid nodes printers types
  where
    cat = List.intercalate (WT.toWrappedTextSingleton " ")

leftPadding :: AlignmenterBuilder
leftPadding c n | n > -1 = padding c (- n)
leftPadding c n = noPadding c n

rightPadding :: AlignmenterBuilder
rightPadding c n | n > -1 = padding c n
rightPadding c n = noPadding c n

noPadding :: AlignmenterBuilder
noPadding _ _ = id

padding :: AlignmenterBuilder
padding c n ys
  | n' > len = padding' c padSize ys
  | otherwise = ys
  where
    n' = abs n
    len = sum $ map WT.wtLengthForDisplay ys
    padSize = signum n * (n' - len)

padding' :: AlignmenterBuilder
padding' c n t = case n `compare` 0 of
  GT -> t <> pad
  LT -> pad <> t
  EQ -> t
  where
    pad = WT.toWrappedTextSingleton $ T.replicate (abs n) c
