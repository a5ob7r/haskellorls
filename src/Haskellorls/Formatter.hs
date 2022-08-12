module Haskellorls.Formatter
  ( Printer,
    Printers (..),
    PrinterType (..),
    buildPrinters,
    buildLines,
    buildPrinterTypes,
  )
where

import Data.Default.Class
import Data.Foldable
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Haskellorls.Class
import qualified Haskellorls.Config as Config
import Haskellorls.Config.DeviceNumber
import qualified Haskellorls.Config.Format as Format
import qualified Haskellorls.Config.Indicator as Indicator
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.Context as Context
import qualified Haskellorls.Formatter.Filemode as Filemode
import qualified Haskellorls.Formatter.Icon as Icon
import qualified Haskellorls.Formatter.Indicator as Indicator
import qualified Haskellorls.Formatter.Inode as Inode
import qualified Haskellorls.Formatter.Link as Link
import qualified Haskellorls.Formatter.Name as Name
import qualified Haskellorls.Formatter.Ownership as Ownership
import qualified Haskellorls.Formatter.Size as Size
import qualified Haskellorls.Formatter.SymbolicLink as SymbolicLink
import qualified Haskellorls.Formatter.Time as Time
import qualified Haskellorls.Formatter.Tree as Tree
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node

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

neededNamePrinterTypeBy :: NamePrinterType -> Config.Config -> Bool
neededNamePrinterTypeBy npType config = case npType of
  TREE -> Config.tree config
  ICON -> Config.icon config
  NAME -> True
  LINK -> Config.isLongStyle config
  INDICATOR -> Indicator.IndicatorNone < Config.indicatorStyle config

buildNamePrinterTypes :: Config.Config -> [NamePrinterType]
buildNamePrinterTypes config = filter (`neededNamePrinterTypeBy` config) [TREE, ICON, NAME, LINK, INDICATOR]

buildNodeNamePrinter :: Config.Config -> NodeNamePrinters -> Printer
buildNodeNamePrinter config printers node = concatMap (\npType -> nodeNamePrinterSelector npType printers node) $ buildNamePrinterTypes config

type Printer = Node.NodeInfo -> [Attr.Attribute WT.WrappedText]

data Printers = Printers
  { fileInodePrinter :: Printer,
    fileBlockPrinter :: Printer,
    fileFieldPrinter :: Printer,
    fileLinkPrinter :: Printer,
    fileOwnerPrinter :: Printer,
    fileGroupPrinter :: Printer,
    fileContextPrinter :: Printer,
    fileSizePrinter :: (Int, Int) -> Printer,
    fileTimePrinter :: Printer,
    fileNamePrinter :: Printer,
    fileNameWithDQuotePrinter :: Printer
  }

buildPrinters :: Config.Config -> IO Printers
buildPrinters config = do
  lscolors <-
    if Config.colorize config
      then Color.lsColors
      else return def
  lsicons <-
    if Config.icon config
      then Color.lsIcons
      else return def
  uidSubstTable <-
    if Config.format config == Format.LONG && not (Config.numericUidGid config) && (Config.owner config || Config.author config)
      then Ownership.getUserIdSubstTable
      else pure mempty
  gidSubstTable <-
    if Config.format config == Format.LONG && not (Config.numericUidGid config) && Config.group config
      then Ownership.getGroupIdSubstTable
      else pure mempty
  userInfo <- Ownership.userInfo
  currentTime <- getCurrentTime
  timeZone <- getCurrentTimeZone

  let shouldColorize = Config.colorize config
      isEnableExtraColor = Config.extraColor config

      fileInodePrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Inode.nodeInodeNumberWithColor lscolors
          (True, _) -> Inode.nodeInodeNumberWithNormalColor lscolors
          _ -> (\t -> [Attr.Other $ WT.deserialize t]) . T.pack . show . Inode.nodeInodeNumber

      fileBlockPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Size.coloredFileBlockSize lscolors config
          (True, _) -> Size.normalColoredFileBlockSize lscolors config
          _ -> Size.fileBlockSize config

      filemodeFieldPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Filemode.showFilemodeFieldWithColor lscolors
          (True, _) -> Filemode.showFilemodeFieldWithNormalColor lscolors
          _ -> Filemode.showFilemodeField

      fileLinkPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Link.nodeLinksNumberWithColor lscolors
          (True, _) -> Link.nodeLinksNumberWithNormalColor lscolors
          _ -> (\t -> [Attr.Other $ WT.deserialize t]) . T.pack . show . Link.nodeLinksNumber

      fileOwnerPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Ownership.coloredOwnerName uidSubstTable lscolors userInfo
          (True, _) -> Ownership.normalColoredOwnerName uidSubstTable lscolors
          _ -> (\t -> [Attr.Other $ WT.deserialize t]) . Ownership.ownerName uidSubstTable

      fileGroupPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Ownership.coloredGroupName gidSubstTable lscolors userInfo
          (True, _) -> Ownership.normalColoredGroupName gidSubstTable lscolors
          _ -> (\t -> [Attr.Other $ WT.deserialize t]) . Ownership.groupName gidSubstTable

      fileContextPrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Context.colorizedContext lscolors
          (True, _) -> Context.normalColorizedContext lscolors
          _ -> (\t -> [Attr.Other $ WT.deserialize t]) . Context.context

      fileSizePrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> \w -> Size.coloredFileSize w lscolors config
          (True, _) -> \w -> Size.normalColoredFileSize w lscolors config
          _ -> (`Size.fileSize` config)

      fileTimePrinter =
        case (shouldColorize, isEnableExtraColor) of
          (True, True) -> Time.coloredTimeStyleFunc lscolors timeZone defaultTimeLocale currentTime timeStyle . fileTime
          (True, _) -> Time.normalColoredTimeStyleFunc lscolors timeZone defaultTimeLocale currentTime timeStyle . fileTime
          _ -> (\t -> [Attr.Other $ WT.deserialize t]) . timeStyleFunc . fileTime
        where
          timeStyleFunc = Time.timeStyleFunc timeZone defaultTimeLocale currentTime timeStyle
          fileTime = posixSecondsToUTCTime . Node.fileTime
          timeStyle = Config.timeStyle config

      -- TODO: Should use colored icon? But, must consider charactor size and background color.
      -- e.g. $ echo -e '\e[48;5;196;38;5;232;1m\e[0m'
      nodeTreePrinter =
        if shouldColorize
          then Tree.treeBranchWithColor lscolors . Node.getTreeNodePositions
          else (\t -> [Attr.Other $ WT.deserialize t]) . Tree.treeBranch . Node.getTreeNodePositions
      nodeIconPrinter = flip Icon.lookupIcon lsicons
      buildNamePrinter conf =
        if shouldColorize
          then Name.colorizedNodeNameWrapper conf lscolors
          else Name.nodeNameWrapper conf
      nodeNamePrinter = buildNamePrinter config {Config.noQuote = True}
      nodeNameWithDQuotePrinter = buildNamePrinter config
      nodeIndicatorPrinter = Indicator.buildIndicatorPrinter config
      nodeLinkPrinter =
        if shouldColorize
          then SymbolicLink.coloredLinkName config lscolors
          else SymbolicLink.linkName config
      nodeNamePrinters = NodeNamePrinters {..}
      nodeNameWithDQuotePrinters = NodeNamePrinters {nodeNamePrinter = nodeNameWithDQuotePrinter, ..}

  return $
    Printers
      { fileFieldPrinter = filemodeFieldPrinter . from,
        fileNamePrinter = buildNodeNamePrinter config nodeNamePrinters,
        fileNameWithDQuotePrinter = buildNodeNamePrinter config nodeNameWithDQuotePrinters,
        ..
      }

buildPrinterTypes :: Config.Config -> [PrinterType]
buildPrinterTypes config = filter (`neededBy` config) [FILEINODE, FILEBLOCK, FILEFIELD, FILELINK, FILEOWNER, FILEGROUP, FILEAUTHOR, FILECONTEXT, FILESIZE, FILETIME, FILENAME, FILENAMEWITHDQUOTE]

-- | Should the `PrinterType` value is needed by the options.
neededBy :: PrinterType -> Config.Config -> Bool
neededBy pType config = case pType of
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
    inode = Config.inode config
    size = Config.size config
    long = Config.isLongStyle config
    owner = Config.owner config
    group = Config.group config
    author = Config.author config
    context = Config.context config
    noQuote = Config.noQuote config

buildColumn :: [Node.NodeInfo] -> Printers -> PrinterType -> [[Attr.Attribute WT.WrappedText]]
buildColumn nodes printers pType = justify <$> nodes'
  where
    format =
      case pType of
        FILEINODE -> fileInodePrinter printers
        FILEBLOCK -> fileBlockPrinter printers
        FILEFIELD -> fileFieldPrinter printers
        FILELINK -> fileLinkPrinter printers
        FILEOWNER -> fileOwnerPrinter printers
        FILEGROUP -> fileGroupPrinter printers
        FILEAUTHOR -> fileOwnerPrinter printers
        FILECONTEXT -> fileContextPrinter printers
        FILESIZE ->
          let majorWidth = maximum $ 0 : (length . show . unMajorID . from . Node.specialDeviceID <$> nodes)
              minorWidth = maximum $ 0 : (length . show . unMinorID . from . Node.specialDeviceID <$> nodes)
           in fileSizePrinter printers (majorWidth, minorWidth)
        FILETIME -> fileTimePrinter printers
        FILENAME -> fileNamePrinter printers
        FILENAMEWITHDQUOTE -> fileNameWithDQuotePrinter printers
    nodes' = format <$> nodes
    l = maximum $ 0 : (sum . map (termLength . Attr.unwrap) <$> nodes')
    justify =
      case pType of
        FILEINODE -> justifyRight l ' '
        FILEBLOCK -> justifyRight l ' '
        FILEFIELD -> id
        FILELINK -> justifyRight l ' '
        FILEOWNER -> justifyLeft l ' '
        FILEGROUP -> justifyLeft l ' '
        FILEAUTHOR -> justifyLeft l ' '
        FILECONTEXT -> justifyLeft l ' '
        FILESIZE -> justifyRight l ' '
        FILETIME -> justifyLeft l ' '
        FILENAME -> id
        FILENAMEWITHDQUOTE -> id

buildGrid :: [Node.NodeInfo] -> Printers -> [PrinterType] -> [[[Attr.Attribute WT.WrappedText]]]
buildGrid nodes printers = L.transpose . map (buildColumn nodes printers)

buildLines :: Foldable t => t Node.NodeInfo -> Printers -> [PrinterType] -> [[Attr.Attribute WT.WrappedText]]
buildLines nodes printers types = L.intercalate [Attr.Other $ WT.deserialize " "] <$> buildGrid (toList nodes) printers types

justifyLeft :: Int -> Char -> [Attr.Attribute WT.WrappedText] -> [Attr.Attribute WT.WrappedText]
justifyLeft n c wt =
  case n - l of
    n' | n' > 0 -> wt <> [Attr.Other . deserialize . T.replicate n' $ T.singleton c]
    _ -> wt
  where
    l = sum $ termLength . Attr.unwrap <$> wt

justifyRight :: Int -> Char -> [Attr.Attribute WT.WrappedText] -> [Attr.Attribute WT.WrappedText]
justifyRight n c wt =
  case n - l of
    n' | n' > 0 -> (Attr.Other . deserialize . T.replicate n' $ T.singleton c) : wt
    _ -> wt
  where
    l = sum $ termLength . Attr.unwrap <$> wt
