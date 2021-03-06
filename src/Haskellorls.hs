{-# LANGUAGE OverloadedStrings #-}

module Haskellorls
  ( run,
    argParser,
  )
where

import qualified Data.Either as E
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Quote.Utils as Quote
import qualified Haskellorls.Recursive as Recursive
import qualified Haskellorls.Size.Utils as Size
import qualified Haskellorls.Utils as Utils
import qualified Options.Applicative as OA
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
    then showVersion
    else run' options {Option.blockSize = blockSize, Option.quotingStyle = quotingStyle, Option.toStdout = isConnectedToTerminal}

run' :: Option.Option -> IO ()
run' opt = do
  -- Assumes that current directory path is passed as argument implicitly if no argument.
  let targets = (\ss -> if null ss then ["."] else ss) $ Option.targets opt

      -- Only dereferences on command line arguments.
      opt' = opt {Option.dereferenceCommandLine = False, Option.dereferenceCommandLineSymlinkToDir = False}

  statuses <- mapM Utils.getSymbolicLinkStatus targets
  let (errs, exists) = E.partitionEithers $ zipWith (\s p -> E.either Left (const $ Right p) s) statuses targets

  mapM_ (IO.hPrint IO.stderr) errs

  printers <- Decorator.buildPrinters opt'
  let printer = Recursive.buildPrinter opt' printers

  (inodeSet, ops) <- Recursive.buildInitialOperations opt exists

  Recursive.exec inodeSet printer ops

  if null errs
    then Exit.exitSuccess
    else Exit.exitWith $ Exit.ExitFailure 2

argParser :: [String] -> IO Option.Option
argParser args = OA.handleParseResult presult
  where
    presult = OA.execParserPure pprefs pinfo args
    pprefs = OA.prefs OA.helpLongEquals
    pinfo = Option.opts

showVersion :: IO ()
showVersion = T.putStrLn version

version :: T.Text
version = "v0.4.0.0"
