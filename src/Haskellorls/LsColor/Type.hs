module Haskellorls.LsColor.Type
  ( LsColorDict (..),
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

newtype LsColorDict = LsColorDict {getLsColorDict :: M.Map T.Text T.Text}
