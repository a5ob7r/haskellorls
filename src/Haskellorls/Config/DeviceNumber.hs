module Haskellorls.Config.DeviceNumber
  ( MajorID (..),
    MinorID (..),
  )
where

import qualified Data.Text as T
import Haskellorls.System.Posix.Files.ByteString
import System.Posix.Types
import Witch (From (..))

-- | The major ID of a device.
newtype MajorID = MajorID {unMajorID :: DeviceID}

instance From DeviceID MajorID where
  from = MajorID . major

instance From MajorID T.Text where
  from = T.pack . show . unMajorID

-- | The minor ID of a device.
newtype MinorID = MinorID {unMinorID :: DeviceID}

instance From DeviceID MinorID where
  from = MinorID . minor

instance From MinorID T.Text where
  from = T.pack . show . unMinorID
