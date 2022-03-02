module Haskellorls
  ( run,
    argParser,
  )
where

import qualified Data.Either as E
import Data.Version (showVersion)
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Recursive as Recursive
import qualified Haskellorls.Size.Utils as Size
import qualified Haskellorls.Utils as Utils
import qualified Options.Applicative as OA
import Paths_haskellorls (version)
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | Haskellorls's process flow
-- 1. Gets all arguments passed to itself as string list.
-- 2. Parses the arguments and converts them into an 'option' record.
-- 3. Gets 'files' record from the 'option' record's 'target' attribute.
-- 4. Converts the 'files' record into 'entry' record list.
-- 5. Build file status 'printers' from the 'option' record.
-- 6. Build 'entry' printer form the 'option' record and 'printers'.
-- 7. Print all 'entries' using the 'entry' printer.
run :: [String] -> IO ()
run args = do
  options <- argParser args

  isConnectedToTerminal <- IO.hIsTerminalDevice IO.stdout
  blockSize <- Size.lookupBlockSize options
  quotingStyle <- Quote.lookupQuotingStyle options

  if Option.version options
    then putStrLn versionFromCabal
    else run' options {Option.blockSize = blockSize, Option.quotingStyle = quotingStyle, Option.toStdout = isConnectedToTerminal}

run' :: Option.Option -> IO ()
run' opt = do
  -- Assumes that current directory path is passed as argument implicitly if no argument.
  let targets = (\ss -> if null ss then ["."] else ss) $ Option.targets opt

      -- Only dereferences on command line arguments.
      opt' = opt {Option.dereferenceCommandLine = False, Option.dereferenceCommandLineSymlinkToDir = False}

  statuses <- mapM Utils.getSymbolicLinkStatus targets
  let (errs, exists) = E.partitionEithers $ zipWith (\s p -> s >>= const (Right p)) statuses targets

  mapM_ (IO.hPrint IO.stderr) errs

  printers <- Decorator.buildPrinters opt'
  let printer = Recursive.buildPrinter opt' printers
      c = Recursive.LsConf (opt, printer)

  (inodes, ops) <- Recursive.buildInitialOperations c exists

  let s = Recursive.LsState (ops, inodes)

  _ <- Recursive.run c s

  if null errs
    then Exit.exitSuccess
    else Exit.exitWith $ Exit.ExitFailure 2

argParser :: [String] -> IO Option.Option
argParser args = OA.handleParseResult presult
  where
    presult = OA.execParserPure pprefs pinfo args
    pprefs = OA.prefs OA.helpLongEquals
    pinfo = Option.opts

-- Get version info from .cabal.
versionFromCabal :: String
versionFromCabal = showVersion version
