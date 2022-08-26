module Haskellorls.Data.Gettext.Extra (lookupMO) where

import Control.Applicative (Alternative, optional)
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (asum)
import Data.List (unfoldr)
import Data.List.Extra (breakEnd, split)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.FilePath.Posix.ByteString (RawFilePath, encodeFilePath, (</>))
import System.Posix.Files.ByteString (fileAccess)

-- | Find an appropriate gettext(.mo)'s filepath.
lookupMO :: (Alternative m, MonadThrow m, MonadIO m) => RawFilePath -> String -> String -> m (Maybe RawFilePath)
lookupMO localedir locale domainname = do
  locales <- case locale of
    "C" -> return [locale]
    _ -> do
      locales <- split (== ':') <$> liftIO (fromMaybe "" <$> lookupEnv "LANGUAGE")
      return . filter (not . null) $ locales <> [locale]
  optional . asum $ (\lc -> f $ localedir </> encodeFilePath lc </> "LC_MESSAGES" </> encodeFilePath domainname <> ".mo") <$> mconcat (languages <$> locales)
  where
    readable path = liftIO $ fileAccess path True False False
    f path = readable path >>= \b -> if b then return path else throwString "No a readable permission."
    languages = unfoldr $ \lang -> case breakEnd (`elem` ("@_." :: String)) lang of
      ("", "") -> Nothing
      ("", r) -> Just (r, "")
      (l, _) -> Just (lang, init l)
