module Haskellorls.Formatter
  ( Printers,
    mkPrinters,
    mkLines,
    mkPrinterTypes,
  )
where

import Data.Default.Class
import Data.Either (isLeft)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (intercalate, transpose)
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
import qualified Haskellorls.Formatter.Quote as Quote
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

data NodeNamePrinters = NodeNamePrinters
  { nodeTreePrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    nodeIconPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    nodeNamePrinter :: Node.NodeInfo -> Attr.Attribute WT.WrappedText,
    nodeLinkPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    nodeIndicatorPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
  }

data NodeNames = NodeNames
  { nodeTree :: [Attr.Attribute WT.WrappedText],
    nodeIcon :: [Attr.Attribute WT.WrappedText],
    -- | A "Right" wrapped value is considered as the length is changed by
    -- quotation, otherwise the value is wrapped by "Left".
    nodeName :: Either (Attr.Attribute WT.WrappedText) (Attr.Attribute WT.WrappedText),
    nodeLink :: [Attr.Attribute WT.WrappedText],
    nodeIndicator :: [Attr.Attribute WT.WrappedText]
  }

mkNodeNamePrinter :: Config.Config -> NodeNamePrinters -> Node.NodeInfo -> NodeNames
mkNodeNamePrinter config NodeNamePrinters {..} node = NodeNames {..}
  where
    nodeTree =
      if Config.tree config
        then nodeTreePrinter node
        else []
    nodeIcon =
      if Config.icon config
        then nodeIconPrinter node
        else []
    nodeName =
      let name = nodeNamePrinter node
          quoted = Quote.quote config name
       in if Config.format config == Format.COMMAS
            then Left quoted
            else case Config.quotingStyle config of
              Quote.Literal -> Left quoted
              Quote.Escape -> Left quoted
              Quote.ShellAlways -> Right quoted
              Quote.ShellEscapeAlways -> Right quoted
              _ -> case T.compareLength (WT.wtWord $ Attr.unwrap quoted) (T.length . WT.wtWord $ Attr.unwrap name) of
                EQ -> Left quoted
                _ -> Right quoted
    nodeLink
      | Format.LONG <- Config.format config = nodeLinkPrinter node
      | otherwise = []
    nodeIndicator =
      if Indicator.IndicatorNone < Config.indicatorStyle config
        then nodeIndicatorPrinter node
        else []

data Printers = Printers
  { fileInodePrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileBlockPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileFieldPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileLinkPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileOwnerPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileGroupPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileContextPrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileSizePrinter :: (Int, Int) -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileTimePrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
    fileNamePrinters :: NodeNamePrinters
  }

mkPrinters :: Config.Config -> IO Printers
mkPrinters config = do
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
          _ -> (\t -> [Attr.Other $ WT.deserialize t]) . Time.timeStyleFunc timeZone defaultTimeLocale currentTime timeStyle . fileTime
        where
          fileTime = posixSecondsToUTCTime . Node.fileTime
          timeStyle = Config.timeStyle config

      -- TODO: Should use colored icon? But, must consider charactor size and background color.
      -- e.g. $ echo -e '\e[48;5;196;38;5;232;1m\e[0m'
      nodeTreePrinter =
        if shouldColorize
          then Tree.treeBranchWithColor lscolors . Node.getTreeNodePositions
          else (\t -> [Attr.Other $ WT.deserialize t]) . Tree.treeBranch . Node.getTreeNodePositions
      nodeIconPrinter = flip Icon.lookupIcon lsicons
      nodeNamePrinter =
        if shouldColorize
          then Name.colorizedNodeName config lscolors
          else Name.nodeName config
      nodeIndicatorPrinter = Indicator.buildIndicatorPrinter config
      nodeLinkPrinter =
        if shouldColorize
          then SymbolicLink.coloredLinkName config lscolors
          else SymbolicLink.linkName config
      fileNamePrinters = NodeNamePrinters {..}

  return $ Printers {fileFieldPrinter = filemodeFieldPrinter . from, ..}

mkPrinterTypes :: Config.Config -> [PrinterType]
mkPrinterTypes config = filter predicate [FILEINODE, FILEBLOCK, FILEFIELD, FILELINK, FILEOWNER, FILEGROUP, FILEAUTHOR, FILECONTEXT, FILESIZE, FILETIME, FILENAME]
  where
    predicate = \case
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
      FILENAME -> True
    inode = Config.inode config
    size = Config.size config
    long = Config.format config == Format.LONG
    owner = Config.owner config
    group = Config.group config
    author = Config.author config
    context = Config.context config

mkColumn :: [Node.NodeInfo] -> Config.Config -> Printers -> PrinterType -> [[Attr.Attribute WT.WrappedText]]
mkColumn nodes config printers pType
  | Format.COMMAS <- Config.format config = column
  | otherwise = justify <$> column
  where
    column =
      case pType of
        FILEINODE -> fileInodePrinter printers <$> nodes
        FILEBLOCK -> fileBlockPrinter printers <$> nodes
        FILEFIELD -> fileFieldPrinter printers <$> nodes
        FILELINK -> fileLinkPrinter printers <$> nodes
        FILEOWNER -> fileOwnerPrinter printers <$> nodes
        FILEGROUP -> fileGroupPrinter printers <$> nodes
        FILEAUTHOR -> fileOwnerPrinter printers <$> nodes
        FILECONTEXT -> fileContextPrinter printers <$> nodes
        FILESIZE ->
          let majorWidth = maximum $ 0 : (length . show . unMajorID . from . Node.specialDeviceID <$> nodes)
              minorWidth = maximum $ 0 : (length . show . unMinorID . from . Node.specialDeviceID <$> nodes)
           in fileSizePrinter printers (majorWidth, minorWidth) <$> nodes
        FILETIME -> fileTimePrinter printers <$> nodes
        FILENAME ->
          let namePrinters = fileNamePrinters printers
              names = mkNodeNamePrinter config namePrinters <$> nodes
              f = \case
                Right name -> [name]
                Left name
                  | all (isLeft . nodeName) names -> [name]
                  | otherwise -> [Attr.Other $ deserialize " ", name]
           in names <&> \NodeNames {..} -> nodeTree <> nodeIcon <> f nodeName <> nodeLink <> nodeIndicator
    l = maximum $ 0 : (sum . map (termLength . Attr.unwrap) <$> column)
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

mkGrid :: [Node.NodeInfo] -> Config.Config -> Printers -> [PrinterType] -> [[[Attr.Attribute WT.WrappedText]]]
mkGrid nodes config printers = transpose . map (mkColumn nodes config printers)

mkLines :: Foldable t => t Node.NodeInfo -> Config.Config -> Printers -> [PrinterType] -> [[Attr.Attribute WT.WrappedText]]
mkLines nodes config printers types = intercalate [Attr.Other $ WT.deserialize " "] <$> mkGrid (toList nodes) config printers types

justifyLeft :: Int -> Char -> [Attr.Attribute WT.WrappedText] -> [Attr.Attribute WT.WrappedText]
justifyLeft n c wt
  | diff <- n - l, diff > 0 = wt <> [Attr.Other . deserialize . T.replicate diff $ T.singleton c]
  | otherwise = wt
  where
    l = sum $ termLength . Attr.unwrap <$> wt

justifyRight :: Int -> Char -> [Attr.Attribute WT.WrappedText] -> [Attr.Attribute WT.WrappedText]
justifyRight n c wt
  | diff <- n - l, diff > 0 = (Attr.Other . deserialize . T.replicate diff $ T.singleton c) : wt
  | otherwise = wt
  where
    l = sum $ termLength . Attr.unwrap <$> wt
