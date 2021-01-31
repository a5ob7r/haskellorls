{-# LANGUAGE RankNTypes #-}

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
import qualified Data.Time.Format as Format
import qualified Haskellorls.Field as Field
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Color as Color
import qualified Haskellorls.Name as Name
import qualified Haskellorls.Ownership as Ownership
import qualified Haskellorls.Size as Size
import qualified Haskellorls.Time as Time
import qualified Haskellorls.Link as Link
import qualified Haskellorls.UserInfo as UserInfo
import qualified Haskellorls.YetAnotherString as YAString
import qualified System.Posix.Time as PTime
import System.IO

data PrinterType
  = FILEFIELD
  | FILELINK
  | FILEOWNER
  | FILEGROUP
  | FILESIZE
  | FILETIME
  | FILENAME

type Printer = Node.NodeInfo -> [YAString.WrapedString]
type Alignmenter = [YAString.WrapedString] -> [YAString.WrapedString]
type AlignmenterBuilder = Char -> Int -> Alignmenter

data AlighmentType
  = NONE
  | LEFT
  | RIGHT

data Printers = Printers
  { fileFieldPrinter :: Printer,
    fileLinkPrinter :: Printer,
    fileOwnerPrinter :: Printer,
    fileGroupPrinter :: Printer,
    fileSizePrinter :: Printer,
    fileTimePrinter :: Printer,
    fileNamePrinter :: Printer
  }

alignmentTypeFor :: PrinterType -> AlighmentType
alignmentTypeFor dType = case dType of
  FILEFIELD -> NONE
  FILELINK -> RIGHT
  FILEOWNER -> LEFT
  FILEGROUP -> LEFT
  FILESIZE -> RIGHT
  FILETIME -> LEFT
  FILENAME -> NONE

printerSelectorFor :: PrinterType -> Printers -> Printer
printerSelectorFor pType = case pType of
  FILEFIELD -> fileFieldPrinter
  FILELINK -> fileLinkPrinter
  FILEOWNER -> fileOwnerPrinter
  FILEGROUP -> fileGroupPrinter
  FILESIZE -> fileSizePrinter
  FILETIME -> fileTimePrinter
  FILENAME -> fileNamePrinter

alignmenterBuilderSelectorFor :: AlighmentType -> AlignmenterBuilder
alignmenterBuilderSelectorFor aType = case aType of
  NONE -> noPadding
  LEFT -> rightPadding
  RIGHT -> leftPadding

buildPrinters :: Option.Option -> IO Printers
buildPrinters opt = do
  cConfig <- Color.config
  uidSubstTable <- Ownership.getUserIdSubstTable
  gidSubstTable <- Ownership.getGroupIdSubstTable
  userInfo <- UserInfo.userInfo
  currentTime <- PTime.epochTime
  shouldColorize <- case Option.color opt of
    Option.NEVER -> return False
    Option.ALWAYS -> return True
    Option.AUTO -> hIsTerminalDevice stdout
  let nodePrinter =
        if shouldColorize
          then Name.colorizedNodeName cConfig
          else YAString.toWrappedStringArray . Name.nodeName
      filemodeFieldPrinter =
        if shouldColorize
          then Field.showFilemodeFieldWithColor cConfig
          else Field.showFilemodeField
      fileLinkFieldPrinter =
        if shouldColorize
          then Link.nodeLinksNumberWithColor cConfig
          else YAString.toWrappedStringArray . show . Link.nodeLinksNumber
      fileOwnerFieldPrinter =
        if shouldColorize
          then Ownership.coloredOwnerName uidSubstTable cConfig userInfo
          else YAString.toWrappedStringArray . Ownership.ownerName uidSubstTable
      fileGroupFieldPrinter =
        if shouldColorize
          then Ownership.coloredGroupName gidSubstTable cConfig userInfo
          else YAString.toWrappedStringArray . Ownership.groupName gidSubstTable
      fileSizeType = Size.blockSizeTypeFrom modeStr
        where
          modeStr
            | bSize /= "" = bSize
            | Option.humanReadable opt = "HUMANi"
            | otherwise = ""
          bSize = Option.blockSize opt
      fileSizeFieldPrinter =
        if shouldColorize
          then Size.coloredFileSizeFuncFor fileSizeType cConfig
          else YAString.toWrappedStringArray . Size.fileSizeFuncFor fileSizeType
      fileTimeFileldPrinter =
        if shouldColorize
          then Time.coloredTimeStyleFunc cConfig Format.defaultTimeLocale currentTime timeStyle . fileTime . Node.nodeInfoStatus
          else YAString.toWrappedStringArray . timeStyleFunc . fileTime . Node.nodeInfoStatus
        where
          timeStyleFunc = Time.timeStyleFunc Format.defaultTimeLocale currentTime timeStyle
          fileTime = Time.fileTime . Time.timeTypeFrom $ Option.time opt
          timeStyle = Time.timeStyleFrom $ Option.timeStyle opt
  return $
    Printers
      { fileFieldPrinter = filemodeFieldPrinter . Field.filemodeField . Node.nodeInfoStatus,
        fileLinkPrinter = fileLinkFieldPrinter,
        fileOwnerPrinter = fileOwnerFieldPrinter,
        fileGroupPrinter = fileGroupFieldPrinter,
        fileSizePrinter = fileSizeFieldPrinter,
        fileTimePrinter = fileTimeFileldPrinter,
        fileNamePrinter = nodePrinter
      }

buildPrinterTypes :: Option.Option -> [PrinterType]
buildPrinterTypes opt
  | isLongWithoutOwnerAndGroup = [FILEFIELD, FILELINK, FILESIZE, FILETIME, FILENAME]
  | isLongWithoutGroup = [FILEFIELD, FILELINK, FILEOWNER, FILESIZE, FILETIME, FILENAME]
  | isLongWithoutOwner = [FILEFIELD, FILELINK, FILEGROUP, FILESIZE, FILETIME, FILENAME]
  | long = [FILEFIELD, FILELINK, FILEOWNER, FILEGROUP, FILESIZE, FILETIME, FILENAME]
  | oneline = [FILENAME]
  | otherwise = [FILENAME]
  where
    isLongWithoutOwnerAndGroup = isLongWithoutOwner && (isLongWithoutGroup || noGroup)
    isLongWithoutGroup = longWithoutGroup || long && noGroup
    isLongWithoutOwner = longWithoutOwner
    long = Option.long opt
    oneline = Option.oneline opt
    noGroup = Option.noGroup opt
    longWithoutGroup = Option.longWithoutGroup opt
    longWithoutOwner = Option.longWithoutOwner opt

buildColumn :: [Node.NodeInfo] -> Printers -> PrinterType -> [[YAString.WrapedString]]
buildColumn nodes printers pType = map alignmenter nodes'
  where
    printer = printerSelectorFor pType printers
    nodes' = map printer nodes
    maxLen = YAString.maximumLength nodes'
    aType = alignmentTypeFor pType
    aBuilder = alignmenterBuilderSelectorFor aType
    alignmenter = aBuilder ' ' maxLen

buildGrid :: [Node.NodeInfo] -> Printers -> [PrinterType] -> [[[YAString.WrapedString]]]
buildGrid nodes printers = List.transpose . map (buildColumn nodes printers)

buildLines :: [Node.NodeInfo] -> Printers -> [PrinterType] -> [[YAString.WrapedString]]
buildLines nodes printers types = map cat $ buildGrid nodes printers types
  where
    cat = List.intercalate (YAString.toWrappedStringArray " ")

leftPadding :: AlignmenterBuilder
leftPadding c n | n > -1 = padding c (-n)
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
    len = YAString.yaLength ys
    padSize = signum n * (n' - len)

padding' :: AlignmenterBuilder
padding' c n ys
  | n > 0 = ys ++ pad
  | n < 0 = pad ++ ys
  | otherwise = ys
  where
    pad = YAString.toWrappedStringArray $ replicate (abs n) c
