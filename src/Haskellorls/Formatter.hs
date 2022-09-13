module Haskellorls.Formatter
  ( Printers,
    mkPrinters,
    mkLines,
    mkBlockSizeHeader,
    mkPrinterTypes,
  )
where

import Data.Default.Class
import Data.Either (isLeft)
import Data.Foldable
import Data.Functor ((<&>))
import Data.List (intercalate, transpose)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Haskellorls.Class (termLength)
import qualified Haskellorls.Config as Config
import Haskellorls.Config.DeviceNumber
import Haskellorls.Config.Filetime (Filetime (..))
import qualified Haskellorls.Config.Format as Format
import qualified Haskellorls.Config.Indicator as Indicator
import Haskellorls.Config.Inode (Inode (..))
import Haskellorls.Config.Link (LinkCount (..))
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.Context as Context
import qualified Haskellorls.Formatter.Filemode as Filemode
import qualified Haskellorls.Formatter.Indicator as Indicator
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
import Haskellorls.System.Locale (LcTime (..), lcTime)
import System.Locale.Current (currentLocale)
import System.Locale.LocaleConv (localeConv)
import Witch (from)

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
  { blockSizeHeaderPrinter :: [Node.NodeInfo] -> [Attr.Attribute WT.WrappedText],
    fileInodePrinter :: Node.NodeInfo -> [Attr.Attribute WT.WrappedText],
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
  lscolors <- case Config.colorize config of
    Just _ -> Color.lsColors
    _ -> return def
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
  lctime <- case Config.format config of
    Format.LONG -> lcTime
    _ -> return $ LcTime Nothing
  timeLocale <- case Config.format config of
    Format.LONG -> currentLocale
    _ -> return defaultTimeLocale
  nconfig <- from <$> localeConv

  let blockSizeHeaderPrinter node = [Attr.Other . from . T.concat . ("total " :) . map (from . Attr.unwrap) . Size.toTotalBlockSize config nconfig $ fromMaybe 0 . Node.fileSize <$> node]

      fileInodePrinter = case Config.colorize config of
        Just True -> \node ->
          let inode = maybe (Attr.Missing "?") (Attr.Other . T.pack . show) $ Node.fileID node
              getter = maybe (const Nothing) (Color.lookup . Inode) $ Node.fileID node
           in [WT.wrap lscolors getter <$> inode]
        Just False -> \node ->
          let inode = maybe (Attr.Missing "?") (Attr.Other . T.pack . show) $ Node.fileID node
           in [WT.wrap lscolors Color.normal <$> inode]
        _ -> maybe [Attr.Missing $ from @T.Text "?"] (\inode -> [Attr.Other . from . T.pack $ show inode]) . Node.fileID

      fileBlockPrinter = case Config.colorize config of
        Just True -> Size.coloredFileBlockSize lscolors config nconfig
        Just False -> Size.normalColoredFileBlockSize lscolors config nconfig
        _ -> Size.fileBlockSize config nconfig

      fileFieldPrinter = case Config.colorize config of
        Just True -> Filemode.showFilemodeFieldWithColor lscolors . from
        Just False -> Filemode.showFilemodeFieldWithNormalColor lscolors . from
        _ -> Filemode.showFilemodeField . from

      fileLinkPrinter = case Config.colorize config of
        Just True -> \node ->
          let getter = maybe (const Nothing) (Color.lookup . LinkCount) $ Node.linkCount node
           in maybe [Attr.Missing $ from @T.Text "?"] (\l -> [Attr.Other $ WT.wrap lscolors getter . T.pack $ show l]) $ Node.linkCount node
        Just False -> maybe [Attr.Missing $ from @T.Text "?"] (\l -> [Attr.Other $ WT.wrap lscolors Color.normal . T.pack $ show l]) . Node.linkCount
        _ -> maybe [Attr.Missing $ from @T.Text "?"] (\l -> [Attr.Other $ from . T.pack $ show l]) . Node.linkCount

      fileOwnerPrinter = case Config.colorize config of
        Just True -> Ownership.coloredOwnerName uidSubstTable lscolors userInfo
        Just False -> Ownership.normalColoredOwnerName uidSubstTable lscolors
        _ -> (\t -> [Attr.Other $ from t]) . Ownership.ownerName uidSubstTable

      fileGroupPrinter = case Config.colorize config of
        Just True -> Ownership.coloredGroupName gidSubstTable lscolors userInfo
        Just False -> Ownership.normalColoredGroupName gidSubstTable lscolors
        _ -> (\t -> [Attr.Other $ from t]) . Ownership.groupName gidSubstTable

      fileContextPrinter = case Config.colorize config of
        Just True -> Context.colorizedContext lscolors
        Just False -> Context.normalColorizedContext lscolors
        _ -> (\t -> [Attr.Other $ from t]) . Context.context

      fileSizePrinter = case Config.colorize config of
        Just True -> \w -> Size.coloredFileSize w lscolors config nconfig
        Just False -> \w -> Size.normalColoredFileSize w lscolors config nconfig
        _ -> (\n -> Size.fileSize n config nconfig)

      fileTimePrinter =
        let timeStyle = from (Config.timeStyle config, lctime)
            fmtTime = from . Time.formatFiletime timeZone timeLocale currentTime timeStyle . posixSecondsToUTCTime
         in case Config.colorize config of
              Just True -> maybe [Attr.Missing $ from @T.Text "?"] (\t -> [Attr.Other . WT.wrap lscolors (Color.lookup $ Filetime ()) $ fmtTime t]) . Node.fileTime
              Just False -> maybe [Attr.Missing $ from @T.Text "?"] (\t -> [Attr.Other . WT.wrap lscolors Color.normal $ fmtTime t]) . Node.fileTime
              _ -> maybe [Attr.Missing $ from @T.Text "?"] (\t -> [Attr.Other . from $ fmtTime t]) . Node.fileTime

      nodeTreePrinter = case Config.colorize config of
        Just _ -> Tree.treeBranchWithColor lscolors . Node.getTreeNodePositions
        _ -> (\t -> [Attr.Other $ from t]) . Tree.treeBranch . Node.getTreeNodePositions
      nodeNamePrinter = case Config.colorize config of
        Just _ -> Name.colorizedNodeName config lscolors
        _ -> Name.nodeName config
      nodeIndicatorPrinter = Indicator.mkIndicatorPrinter config
      nodeLinkPrinter = case Config.colorize config of
        Just _ -> SymbolicLink.coloredLinkName config lscolors
        _ -> SymbolicLink.linkName config

  -- TODO: Should we use colored icon? However we must consider charactor size
  -- and background color. e.g. @echo -e '\e[48;5;196;38;5;232;1m\e[0m'@.
  nodeIconPrinter <- do
    lsicons <- if Config.icon config then Color.lsIcons else return def
    return $ \node -> Attr.Other <$> [from . maybe "" Color.unIcon $ node `Color.lookup` lsicons, from @T.Text " "]

  return $ Printers {fileNamePrinters = NodeNamePrinters {..}, ..}

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
          let majorWidth = maximum $ 0 : (maybe 1 (length . show . unMajorID . from) . Node.specialDeviceID <$> nodes)
              minorWidth = maximum $ 0 : (maybe 1 (length . show . unMinorID . from) . Node.specialDeviceID <$> nodes)
           in fileSizePrinter printers (majorWidth, minorWidth) <$> nodes
        FILETIME -> fileTimePrinter printers <$> nodes
        FILENAME ->
          let namePrinters = fileNamePrinters printers
              names = mkNodeNamePrinter config namePrinters <$> nodes
              f = \case
                Right name -> [name]
                Left name
                  | all (isLeft . nodeName) names -> [name]
                  | otherwise -> [Attr.Other $ from @T.Text " ", name]
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
        FILETIME -> \case
          s@[Attr.Missing _] -> justifyRight l ' ' s
          s -> justifyLeft l ' ' s
        FILENAME -> id

mkGrid :: [Node.NodeInfo] -> Config.Config -> Printers -> [PrinterType] -> [[[Attr.Attribute WT.WrappedText]]]
mkGrid nodes config printers = transpose . map (mkColumn nodes config printers)

mkLines :: Foldable t => t Node.NodeInfo -> Config.Config -> Printers -> [PrinterType] -> [[Attr.Attribute WT.WrappedText]]
mkLines nodes config printers types = intercalate [Attr.Other $ from @T.Text " "] <$> mkGrid (toList nodes) config printers types

mkBlockSizeHeader :: Foldable t => t Node.NodeInfo -> Printers -> [Attr.Attribute WT.WrappedText]
mkBlockSizeHeader nodes printers = blockSizeHeaderPrinter printers $ toList nodes

justifyLeft :: Int -> Char -> [Attr.Attribute WT.WrappedText] -> [Attr.Attribute WT.WrappedText]
justifyLeft n c wt
  | diff <- n - l, diff > 0 = wt <> [Attr.Other . from . T.replicate diff $ T.singleton c]
  | otherwise = wt
  where
    l = sum $ termLength . Attr.unwrap <$> wt

justifyRight :: Int -> Char -> [Attr.Attribute WT.WrappedText] -> [Attr.Attribute WT.WrappedText]
justifyRight n c wt
  | diff <- n - l, diff > 0 = (Attr.Other . from . T.replicate diff $ T.singleton c) : wt
  | otherwise = wt
  where
    l = sum $ termLength . Attr.unwrap <$> wt
