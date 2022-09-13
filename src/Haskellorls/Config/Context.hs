module Haskellorls.Config.Context (FileContext (..)) where

newtype FileContext a = FileContext {unFileContext :: a}
