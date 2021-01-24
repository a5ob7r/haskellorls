module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import System.IO
import qualified Options.Applicative as OA
import qualified Data.Time.Format as Format
import qualified System.Posix.Time as PTime

import qualified Haskellorls.Entry as Entry
import qualified Haskellorls.Sort as Sort
import qualified Haskellorls.Color as Color
import Haskellorls.Decorator
import qualified Haskellorls.Field as Field
import qualified Haskellorls.Grid as Grid
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Size as Size
import qualified Haskellorls.Time as Time
import qualified Haskellorls.Ownership as Ownership
import qualified Haskellorls.UserInfo as UserInfo
import qualified Haskellorls.YetAnotherString as YAString (WrapedString (..))

main :: IO ()
main = do
  options <- OA.execParser Option.opts
  run options

run :: Option.Option -> IO ()
run opt = do
  let path = fromMaybe "." . listToMaybe $ Option.targets opt
  contents <- Entry.listContents opt path
  nodes <- fmap (Sort.sorter opt) . mapM Node.nodeInfo $ contents
  cConfig <- Color.config
  uidSubstTable <- Ownership.getUserIdSubstTable
  gidSubstTable <- Ownership.getGroupIdSubstTable
  userInfo <- UserInfo.userInfo
  currentTime <- PTime.epochTime
  shouldColorize <- case Option.color opt of
                      Option.NEVER -> return False
                      Option.ALWAYS -> return True
                      Option.AUTO -> hIsTerminalDevice stdout
  let additionals = decorator nodes fs
      nodePrinter = if shouldColorize
                       then Color.colorizedNodeName cConfig
                       else Color.nodeName
      filemodeFieldPrinter = if shouldColorize
                                then Field.showFilemodeFieldWithColor cConfig
                                else Field.showFilemodeField
      fileOwnerFieldPrinter = if shouldColorize
                                 then Ownership.coloredOwnerName uidSubstTable cConfig userInfo
                                 else toWrappedStringArray . Ownership.ownerName uidSubstTable
      fileGroupFieldPrinter = if shouldColorize
                                 then Ownership.coloredGroupName gidSubstTable cConfig userInfo
                                 else toWrappedStringArray . Ownership.groupName gidSubstTable
      fileSizeType = Size.blockSizeTypeFrom modeStr
        where
          modeStr
            | bSize /= "" = bSize
            | Option.humanReadable opt = "HUMANi"
            | otherwise = ""
          bSize = Option.blockSize opt
      fileSizeFieldPrinter = if shouldColorize
                                then Size.coloredFileSizeFuncFor fileSizeType cConfig
                                else toWrappedStringArray . Size.fileSizeFuncFor fileSizeType
      fileTimeFileldPrinter = if shouldColorize
                                then Time.coloredTimeStyleFunc cConfig Format.defaultTimeLocale currentTime timeStyle . fileTime . Node.nodeInfoStatus
                                else toWrappedStringArray . timeStyleFunc . fileTime . Node.nodeInfoStatus
        where
          timeStyleFunc = Time.timeStyleFunc Format.defaultTimeLocale currentTime timeStyle
          fileTime = Time.fileTime . Time.timeTypeFrom $ Option.time opt
          timeStyle = Time.timeStyleFrom $ Option.timeStyle opt
      nodeNamesForDisplay = map nodePrinter nodes
      fs = [ filemodeFieldPrinter . Field.filemodeField . Node.nodeInfoStatus
           , fileOwnerFieldPrinter
           , fileGroupFieldPrinter
           , fileSizeFieldPrinter
           , fileTimeFileldPrinter
           ]
  if Option.long opt
     then do
       mapM_ (putStrLn . (\(a, b) -> a ++ " " ++ b)) $ zip additionals nodeNamesForDisplay
     else do
       Grid.showNodesWithGridForm nodes nodePrinter

toWrappedStringArray :: String -> [YAString.WrapedString]
toWrappedStringArray s = [toWrappedString s]

toWrappedString :: String -> YAString.WrapedString
toWrappedString s = YAString.WrapedString
  { YAString.wrappedStringPrefix = ""
  , YAString.wrappedStringMain = s
  , YAString.wrappedStringSuffix = ""
  }
