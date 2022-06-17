module Haskellorls
  ( haskellorls,
    argParser,
  )
where

import Control.Monad
import Data.Either
import Data.Version (showVersion)
import qualified Haskellorls.Decorator as Decorator
import Haskellorls.Exception
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Recursive as Recursive
import qualified Haskellorls.Size.Utils as Size
import qualified Haskellorls.Utils as Utils
import Options.Applicative
import Paths_haskellorls (version)
import System.Exit
import System.FilePath.Posix.ByteString
import System.IO

-- | Haskellorls's process flow
-- 1. Gets all arguments passed to itself as string list.
-- 2. Parses the arguments and converts them into an 'option' record.
-- 3. Gets 'files' record from the 'option' record's 'target' attribute.
-- 4. Converts the 'files' record into 'entry' record list.
-- 5. Build file status 'printers' from the 'option' record.
-- 6. Build 'entry' printer form the 'option' record and 'printers'.
-- 7. Print all 'entries' using the 'entry' printer.
haskellorls :: [String] -> IO ()
haskellorls args = do
  options <- argParser args

  if Option.version options
    then putStrLn $ showVersion version
    else do
      isConnectedToTerminal <- hIsTerminalDevice stdout
      blockSize <- Size.lookupBlockSize options
      quotingStyle <- Quote.lookupQuotingStyle options

      run options {Option.blockSize = blockSize, Option.quotingStyle = quotingStyle, Option.toStdout = isConnectedToTerminal}

run :: Option.Option -> IO ()
run opt = do
  -- Assumes that current directory path is passed as argument implicitly if no argument.
  let targets = map encodeFilePath . (\ss -> if null ss then ["."] else ss) $ Option.targets opt

      -- Only dereferences on command line arguments.
      opt' = opt {Option.dereferenceCommandLine = False, Option.dereferenceCommandLineSymlinkToDir = False}

  statuses <- mapM (tryIO . Utils.getSymbolicLinkStatus) targets
  let (errs, exists) = partitionEithers $ zipWith (\s p -> s >>= const (Right p)) statuses targets

  mapM_ printErr errs

  printers <- Decorator.buildPrinters opt'
  let printer = Recursive.buildPrinter opt' printers
      c = Recursive.LsConf (opt, printer)

  (inodes, ops) <- Recursive.buildInitialOperations c exists

  let s = Recursive.LsState (ops, inodes)

  _ <- Recursive.run c s

  unless (null errs) . exitWith $ ExitFailure 2

argParser :: [String] -> IO Option.Option
argParser args = handleParseResult presult
  where
    presult = execParserPure pprefs pinfo args
    pprefs = prefs helpLongEquals
    pinfo = Option.opts
