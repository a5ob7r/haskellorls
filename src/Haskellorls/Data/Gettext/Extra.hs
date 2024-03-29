module Haskellorls.Data.Gettext.Extra (lookupMO) where

import Control.Applicative (Alternative, optional)
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (asum)
import Data.List (unfoldr)
import Data.List.Extra (breakEnd, split)
import Data.Maybe (fromMaybe)
import Haskellorls.System.OsPath.Posix.Extra (PosixPath, encode, (<.>), (</>))
import System.Environment (lookupEnv)
import System.Posix.Files.PosixString (fileAccess)

-- | Find an appropriate gettext(.mo)'s filepath.
lookupMO :: (Alternative m, MonadThrow m, MonadIO m) => PosixPath -> String -> String -> m (Maybe PosixPath)
lookupMO localedir locale domainname = do
  locales <- case locale of
    "C" -> return [locale]
    _ -> do
      locales <- split (== ':') <$> liftIO (fromMaybe "" <$> lookupEnv "LANGUAGE")
      return . filter (not . null) $ locales <> [locale]
  optional . asum $ (\localename -> let p = localedir </> encode localename </> encode "LC_MESSAGES" </> encode domainname <.> encode "mo" in readable p >>= \b -> if b then return p else throwString "No a readable permission.") <$> mconcat (languages <$> locales)
  where
    readable path = liftIO $ fileAccess path True False False
    languages = unfoldr $ \lang -> case breakEnd (`elem` ("@_." :: String)) lang of
      ("", "") -> Nothing
      ("", r) -> Just (r, "")
      (l, _) -> Just (lang, init l)
