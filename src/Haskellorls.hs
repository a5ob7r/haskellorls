module Haskellorls (haskellorls) where

import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Haskellorls.Decorator as Decorator
import Haskellorls.Exception
import qualified Haskellorls.Format.Grid as Grid
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Recursive as Recursive
import qualified Haskellorls.Size.Utils as Size
import Network.HostName
import Options.Applicative
import Paths_haskellorls (version)
import System.Exit
import System.FilePath.Posix.ByteString
import System.IO
import System.Posix.Directory.ByteString

-- | Run @ls@.
--
-- Return @'ExitSuccess'@ if no error.
--
-- Return @'ExitFailure' 2@ if there are any missing filepaths in command
-- arguments.
--
-- Return @'ExitFailure' 1@ if there are any no permission subdirectories in
-- traversed them.
haskellorls :: [String] -> IO ExitCode
haskellorls args = do
  options <- argParser args

  if Option.version options
    then do
      putStrLn $ showVersion version
      return ExitSuccess
    else do
      isConnectedToTerminal <- hIsTerminalDevice stdout
      blockSize <- Size.lookupBlockSize options
      quotingStyle <- Quote.lookupQuotingStyle options
      cwd <- getWorkingDirectory
      hostname <- T.pack <$> getHostName
      columnSize <- Grid.virtualColumnSize options

      run
        options
          { Option.blockSize = blockSize,
            Option.quotingStyle = quotingStyle,
            Option.toStdout = isConnectedToTerminal,
            Option.currentWorkingDirectory = cwd,
            Option.hostname = hostname,
            Option.columnSize = columnSize
          }

run :: Option.Option -> IO ExitCode
run opt = do
  -- Assumes that current directory path is passed as argument implicitly if no argument.
  let targets = map encodeFilePath . (\ss -> if null ss then ["."] else ss) $ Option.targets opt

      -- Only dereferences on command line arguments.
      opt' = opt {Option.dereferenceCommandLine = False, Option.dereferenceCommandLineSymlinkToDir = False}

  printers <- Decorator.buildPrinters opt'
  let printer = Recursive.buildPrinter opt' printers
      c = Recursive.LsConf (opt, printer)

  (ops, inodes, errs) <- Recursive.mkInitialOperations c targets

  mapM_ printErr errs

  let s = Recursive.LsState (ops, inodes, [])

  (_, Recursive.LsState (_, _, errors)) <- Recursive.run c s

  return $
    if
        -- Serious error.
        | not (null errs) -> ExitFailure 2
        -- Minor error.
        | not (null errors) -> ExitFailure 1
        -- No error.
        | otherwise -> ExitSuccess

argParser :: [String] -> IO Option.Option
argParser args = handleParseResult presult
  where
    presult = execParserPure pprefs pinfo args
    pprefs = prefs helpLongEquals
    pinfo = Option.opts
