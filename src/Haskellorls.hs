{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls
  ( run,
    argParser,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Haskellorls.Decorator as Decorator
import qualified Haskellorls.Option as Option
import qualified Haskellorls.Recursive as Recursive
import qualified Haskellorls.Utils as Utils
import qualified Options.Applicative as OA
import qualified System.Exit as Exit

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

  if Option.version options
    then showVersion
    else run' options

run' :: Option.Option -> IO ()
run' opt = do
  let targets = Option.targets opt
      targets' = if null targets then ["."] else targets

  (noExistences, exists) <- Utils.partitionExistOrNotPathes targets'

  mapM_ Utils.outputNoExistPathErr noExistences

  printers <- Decorator.buildPrinters opt
  let printer = Recursive.buildPrinter opt printers

  ops <- Recursive.buildInitialOperations opt exists

  Recursive.exec printer ops

  if null noExistences
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
version = "v0.2.0.0"
