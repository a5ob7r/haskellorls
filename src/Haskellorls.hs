module Haskellorls (haskellorls) where

import Data.Version (showVersion)
import Haskellorls.Config
import Haskellorls.Config.Environment
import qualified Haskellorls.Config.Option as Option
import Haskellorls.Control.Exception.Safe
import qualified Haskellorls.Formatter as Formatter
import qualified Haskellorls.Walk as Walk
import qualified Haskellorls.Walk.Dired as Dired
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

  printers <-
    Formatter.mkPrinters $
      mkConfig
        env
        -- Only dereferences on command line arguments.
        opt
          { Option.oDereferenceCommandLine = False,
            Option.oDereferenceCommandLineSymlinkToDir = False
          }

  let conf = Walk.LsConf (mkConfig env opt, printers)

  (ops, inodes, errs) <-
    let targets = Option.oTargets opt
     in -- Assume that the current directory's path is passed as an argument
        -- implicitly if no argument.
        Walk.mkInitialOperations conf $ encodeFilePath <$> if null targets then ["."] else targets

  mapM_ printErr errs

  (_, Walk.LsState (_, _, _, errors)) <- Walk.run conf $ Walk.LsState (ops, inodes, Dired.empty, [])

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
