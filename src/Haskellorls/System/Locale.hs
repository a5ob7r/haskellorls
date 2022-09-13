module Haskellorls.System.Locale (LcTime (..), lcTime) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Locale.SetLocale (Category (LC_TIME), setLocale)
import Witch (From (..))

-- | @LC_TIME@.
newtype LcTime = LcTime (Maybe String)

instance From LcTime (Maybe String)

instance From (Maybe String) LcTime

-- | Get the value of @LC_TIME@.
lcTime :: MonadIO m => m LcTime
lcTime = liftIO $ LcTime <$> setLocale LC_TIME Nothing
