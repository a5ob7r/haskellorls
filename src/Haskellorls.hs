module Haskellorls (haskellorls) where

import Data.Version (showVersion)
import Haskellorls.Config
import Haskellorls.Config.Environment
import qualified Haskellorls.Config.Option as Option
import Haskellorls.Control.Exception.Safe
import qualified Haskellorls.Formatter as Formatter
import qualified Haskellorls.Walk as Walk
import Options.Applicative
import Paths_haskellorls (version)
import System.Exit
import System.FilePath.Posix.ByteString

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

  if Option.oVersion options
    then do
      putStrLn $ showVersion version
      return ExitSuccess
    else run options

run :: Option.Option -> IO ExitCode
run opt = do
  env <- mkEnvironment

  -- Assumes that current directory path is passed as argument implicitly if no argument.
  let targets = map encodeFilePath . (\ss -> if null ss then ["."] else ss) $ Option.oTargets opt

      -- Only dereferences on command line arguments.
      opt' = opt {Option.oDereferenceCommandLine = False, Option.oDereferenceCommandLineSymlinkToDir = False}

      config = mkConfig env opt
      config' = mkConfig env opt'

  printers <- Formatter.buildPrinters config'
  let printer = Walk.buildPrinter config' printers
      c = Walk.LsConf (config, printer)

  (ops, inodes, errs) <- Walk.mkInitialOperations c targets

  mapM_ printErr errs

  let s = Walk.LsState (ops, inodes, [])

  (_, Walk.LsState (_, _, errors)) <- Walk.run c s

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
