module Haskellorls.Config.Context (FileContext (..)) where

import qualified Data.Text as T

newtype FileContext = FileContext {unFileContext :: T.Text}
