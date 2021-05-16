{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.LsColor.Util
  ( lookupLsColor,
    findLsColorWithDefault,
    module Haskellorls.LsColor.Type,
  )
where

import qualified Data.List.Extra as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Haskellorls.LsColor.Type hiding (def)

lookupLsColor :: T.Text -> LsColorDict -> T.Text
lookupLsColor = findLsColorWithDefault ""

findLsColorWithDefault :: T.Text -> T.Text -> LsColorDict -> T.Text
findLsColorWithDefault def query LsColorDict {..} = L.headDef def . Maybe.mapMaybe (`M.lookup` getLsColorDict) . reverse . T.tails $ T.toUpper query
