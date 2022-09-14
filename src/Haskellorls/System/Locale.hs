module Haskellorls.System.Locale
  ( LcTime (..),
    lcTime,
    LcMessages (..),
    lcMessages,
    initialize,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Locale.SetLocale (Category (LC_ALL, LC_MESSAGES, LC_TIME), setLocale)
import Witch (From (..))

-- | @LC_TIME@.
newtype LcTime = LcTime (Maybe String)

instance From LcTime (Maybe String)

instance From (Maybe String) LcTime

-- | Get the value of @LC_TIME@.
lcTime :: MonadIO m => m LcTime
lcTime = liftIO $ LcTime <$> setLocale LC_TIME Nothing

-- | @LC_MESSAGES@.
newtype LcMessages = LcMessages (Maybe String)

instance From LcMessages (Maybe String)

instance From (Maybe String) LcMessages

-- | Get the value of @LC_MESSAGES@.
lcMessages :: MonadIO m => m LcMessages
lcMessages = liftIO $ LcMessages <$> setLocale LC_MESSAGES Nothing

-- | Enable to lookup locale configurations by @setLocale@. The default locale
-- is the portable @C@ one if no call this in advance.
initialize :: MonadIO m => m ()
initialize = liftIO . void . setLocale LC_ALL $ Just ""
