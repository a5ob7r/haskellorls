module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory.Extra
import System.IO
import qualified Options.Applicative as OA
import qualified System.Posix.Files as Files (fileOwner, fileGroup)

import qualified Haskellorls.Color as Color
import Haskellorls.Decorator
import qualified Haskellorls.Field as Field
import qualified Haskellorls.Grid as Grid
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Size as Size
import qualified Haskellorls.Time as Time
import qualified Haskellorls.Ownership as Ownership

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
  shouldColorize <- case Option.color opt of
                      Option.NEVER -> return False
                      Option.ALWAYS -> return True
                      Option.AUTO -> hIsTerminalDevice stdout
  let additionals = decorator nodes fs
      nodePrinter = if shouldColorize
                       then Color.colorizedNodeName cConfig
                       else Color.nodeName
      nodeNamesForDisplay = map nodePrinter nodes
      lookupUser = flip Ownership.lookupUserName uidSubstTable
      lookupGroup = flip Ownership.lookupGroupName gidSubstTable
      fs = [ Field.showFilemodeField . Field.filemodeField . Node.nodeInfoStatus
           , lookupUser . Files.fileOwner . Node.nodeInfoStatus
           , lookupGroup . Files.fileGroup . Node.nodeInfoStatus
           , Size.rawFileSize . Node.nodeInfoStatus
           , Time.fileModificationTime . Node.nodeInfoStatus
           ]
  if Option.long opt
     then do
       mapM_ (putStrLn . (\(a, b) -> a ++ " " ++ b)) $ zip additionals nodeNamesForDisplay
     else do
       Grid.showNodesWithGridForm nodes nodePrinter
