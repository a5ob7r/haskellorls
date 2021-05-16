module Haskellorls.LsColor.Type
  ( LsColorDict (..),
    Default (..),
  )
where

import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Text as T

newtype LsColorDict = LsColorDict {getLsColorDict :: M.Map T.Text T.Text}

instance Default LsColorDict where
  def = LsColorDict M.empty
