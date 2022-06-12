module Haskellorls.Decorator
  ( Printer,
    Printers (..),
    PrinterType (..),
    buildPrinters,
    buildLines,
    buildPrinterTypes,
  )
where

import Data.Foldable
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Haskellorls.Class
import qualified Haskellorls.Color.Utils as Color
import qualified Haskellorls.Context as Context
import qualified Haskellorls.Filemode as Filemode
import qualified Haskellorls.Format.Util as Format
import qualified Haskellorls.Icon as Icon
import qualified Haskellorls.Indicator.Decorator as Indicator
import qualified Haskellorls.Indicator.Type as Indicator
import qualified Haskellorls.Inode as Inode
import qualified Haskellorls.Link as Link
import qualified Haskellorls.LsColor as Color
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
  lscolors <- Color.lsColors
  lsicons <- Color.lsIcons
  uidSubstTable <- if Option.numericUidGid opt then pure mempty else Ownership.getUserIdSubstTable
  gidSubstTable <- if Option.numericUidGid opt then pure mempty else Ownership.getGroupIdSubstTable
  userInfo <- Ownership.userInfo
  currentTime <- getCurrentTime
  timeZone <- getCurrentTimeZone

  let shouldColorize = Color.shouldColorize opt
      isEnableExtraColor = Option.extraColor opt

      fileInodePrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Inode.nodeInodeNumberWithColor lscolors
          (True, _) -> Inode.nodeInodeNumberWithNormalColor lscolors
          _ -> (\t -> [WT.deserialize t]) . T.pack . show . Inode.nodeInodeNumber

      fileBlockPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Size.coloredFileBlockSize lscolors opt
          (True, _) -> Size.normalColoredFileBlockSize lscolors opt
          _ -> Size.fileBlockSize opt

      filemodeFieldPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Filemode.showFilemodeFieldWithColor lscolors
          (True, _) -> Filemode.showFilemodeFieldWithNormalColor lscolors
          _ -> Filemode.showFilemodeField

      fileLinkPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Link.nodeLinksNumberWithColor lscolors
          (True, _) -> Link.nodeLinksNumberWithNormalColor lscolors
          _ -> (\t -> [WT.deserialize t]) . T.pack . show . Link.nodeLinksNumber

      fileOwnerPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Ownership.coloredOwnerName uidSubstTable lscolors userInfo
          (True, _) -> Ownership.normalColoredOwnerName uidSubstTable lscolors
          _ -> (\t -> [WT.deserialize t]) . Ownership.ownerName uidSubstTable

      fileGroupPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Ownership.coloredGroupName gidSubstTable lscolors userInfo
          (True, _) -> Ownership.normalColoredGroupName gidSubstTable lscolors
          _ -> (\t -> [WT.deserialize t]) . Ownership.groupName gidSubstTable

      fileContextPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Context.colorizedContext lscolors
          (True, _) -> Context.normalColorizedContext lscolors
          _ -> (\t -> [WT.deserialize t]) . Context.context

      fileSizePrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Size.coloredFileSize lscolors opt
          (True, _) -> Size.normalColoredFileSize lscolors opt
          _ -> Size.fileSize opt

      fileTimePrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Time.coloredTimeStyleFunc lscolors timeZone defaultTimeLocale currentTime timeStyle . fileTime
          (True, _) -> Time.normalColoredTimeStyleFunc lscolors timeZone defaultTimeLocale currentTime timeStyle . fileTime
          _ -> (\t -> [WT.deserialize t]) . timeStyleFunc . fileTime
        where
          timeStyleFunc = Time.timeStyleFunc timeZone defaultTimeLocale currentTime timeStyle
          fileTime = posixSecondsToUTCTime . Node.fileTime
          timeStyle = Time.timeStyle opt

      -- TODO: Should use colored icon? But, must consider charactor size and background color.
      -- e.g. $ echo -e '\e[48;5;196;38;5;232;1m\e[0m'
      nodeTreePrinter =
        if shouldColorize
          then Tree.treeBranchWithColor lscolors . Node.getTreeNodePositions
          else (\t -> [WT.deserialize t]) . Tree.treeBranch . Node.getTreeNodePositions
      nodeIconPrinter = flip Icon.lookupIcon lsicons
      buildNamePrinter option =
        if shouldColorize
          then Name.colorizedNodeNameWrapper option lscolors
          else Name.nodeNameWrapper option
      nodeNamePrinter = buildNamePrinter opt {Option.noQuote = True}
      nodeNameWithDQuotePrinter = buildNamePrinter opt
      nodeIndicatorPrinter = Indicator.buildIndicatorPrinter opt
      nodeLinkPrinter =
        if shouldColorize
          then SymbolicLink.coloredLinkName opt lscolors
          else SymbolicLink.linkName opt
      nodeNamePrinters = NodeNamePrinters {..}
      nodeNameWithDQuotePrinters = NodeNamePrinters {nodeNamePrinter = nodeNameWithDQuotePrinter, ..}

  return $
    Printers
      { fileFieldPrinter = filemodeFieldPrinter . from,
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
    maxLen = maximum $ map (sum . map WT.len) nodes'
    aType = alignmentTypeFor pType
    aBuilder = alignmenterBuilderSelectorFor aType
    alignmenter = aBuilder " " maxLen

buildGrid :: [Node.NodeInfo] -> Printers -> [PrinterType] -> [[[WT.WrappedText]]]
buildGrid nodes printers = L.transpose . map (buildColumn nodes printers)

buildLines :: Foldable t => t Node.NodeInfo -> Printers -> [PrinterType] -> [[WT.WrappedText]]
buildLines nodes printers types = L.intercalate [WT.deserialize " "] <$> buildGrid (toList nodes) printers types

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
  | n' > l = padding' c padSize ys
  | otherwise = ys
  where
    n' = abs n
    l = sum $ map WT.len ys
    padSize = signum n * (n' - l)

padding' :: AlignmenterBuilder
padding' c n t = case n `compare` 0 of
  GT -> t <> pad
  LT -> pad <> t
  EQ -> t
  where
    pad = (: []) . WT.deserialize $ T.replicate (abs n) c
