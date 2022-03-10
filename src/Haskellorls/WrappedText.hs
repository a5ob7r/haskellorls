module Haskellorls.WrappedText
  ( WrappedText (..),
    module Haskellorls.Class,
  )
where

import qualified Data.Text as T
import Haskellorls.Class
import qualified Haskellorls.Utils as Utils

data WrappedText = WrappedText
  { wtPrefix :: T.Text,
    wtWord :: T.Text,
    wtSuffix :: T.Text
  }

instance From WrappedText T.Text where
  from (WrappedText {..}) = wtPrefix <> wtWord <> wtSuffix

instance Serialize WrappedText

instance From T.Text WrappedText where
  from t = WrappedText "" t ""

instance Deserialize WrappedText

instance Length WrappedText where
  len = Utils.textLengthForDisplay . wtWord
