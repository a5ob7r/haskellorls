module Haskellorls (haskellorls) where

import Control.Monad (void)
import Data.Default.Class (Default (..))
import Data.Version (showVersion)
import Haskellorls.Config
import Haskellorls.Config.Environment
import qualified Haskellorls.Config.Option as Option
import qualified Haskellorls.Formatter as Formatter
import qualified Haskellorls.Walk as Walk
import Options.Applicative
import Paths_haskellorls (version)
import System.Exit
import System.FilePath.Posix.ByteString
import System.Locale.SetLocale (Category (LC_ALL), setLocale)

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
  -- This is to lookup @LC_TIME@ by disabling the default portable @C@ locale.
  void . setLocale LC_ALL $ Just ""

  options <- argParser args

  if Option.oVersion options
    then do
      putStrLn $ showVersion version
      return ExitSuccess
    else run options

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
        Walk.runLs conf st . Walk.mkInitialOperations $ encodeFilePath <$> if null targets then ["."] else targets

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
