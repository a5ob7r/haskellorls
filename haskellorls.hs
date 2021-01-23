module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory.Extra
import System.IO
import qualified Options.Applicative as OA

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
  contents <- listContents path
  nodes <- mapM Node.nodeInfo contents
  cConfig <- Color.config
  uidSubstTable <- Ownership.getUserIdSubstTable
  gidSubstTable <- Ownership.getGroupIdSubstTable
  userInfo <- UserInfo.userInfo
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
      fileTimeFileldPrinter = toWrappedStringArray . Time.toString . fileTime . Node.nodeInfoStatus
        where
          fileTime = Time.fileTime . Time.timeTypeFrom $ Option.time opt
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
