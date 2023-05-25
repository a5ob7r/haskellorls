module Haskellorls (haskellorls) where

import Control.Exception.Safe (catch)
import Data.Default.Class (Default (..))
import Haskellorls.Config
import Haskellorls.Config.Environment
import Haskellorls.Config.Option qualified as Option
import Haskellorls.Formatter qualified as Formatter
import Haskellorls.System.Locale qualified as Locale
import Haskellorls.System.OsPath.Posix.Extra (encode)
import Haskellorls.Walk qualified as Walk
import Options.Applicative
import System.Exit

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
  Locale.initialize

  (argParser args >>= run) `catch` return

run :: Option.Option -> IO ExitCode
run opt = do
  env <- mkEnvironment

  let config = mkConfig env opt

  printers <- Formatter.mkPrinters config

  (ops, Walk.LsState inodes indices errors) <-
    let targets = Option.oTargets opt
        conf = Walk.LsConf config printers
        st = def
     in -- Assume that the current directory's path is passed as an argument
        -- implicitly if no argument.
        Walk.runLs conf st . Walk.mkInitialOperations $ encode <$> if null targets then ["."] else targets

  let -- Dereference only on command line arguments.
      conf = Walk.LsConf (disableDereferenceOnCommandLine config) printers
      st = Walk.LsState inodes indices []
   in Walk.run conf st ops >>= \case
        -- Serious error.
        _ | not $ null errors -> return $ ExitFailure 2
        -- Minor error.
        (_, Walk.LsState _ _ (_e : _errs)) -> return $ ExitFailure 1
        -- No error.
        _ -> return ExitSuccess

argParser :: [String] -> IO Option.Option
argParser args = handleParseResult presult
  where
    presult = execParserPure pprefs pinfo args
    pprefs = prefs helpLongEquals
    pinfo = Option.opts
