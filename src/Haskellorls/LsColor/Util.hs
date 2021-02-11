{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.LsColor.Util
  ( lookupLsColor,
  )
where

import qualified Data.List.Extra as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Haskellorls.LsColor.Type

lookupLsColor :: T.Text -> LsColorDict -> T.Text
lookupLsColor query LsColorDict {..} = L.headDef "" . Maybe.mapMaybe (`M.lookup` getLsColorDict) . reverse . T.tails $ T.toUpper query
