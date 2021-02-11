module Haskellorls.LsColor.Parser
  ( colorIndicatorsFrom,
    parametorsFrom,
    module Haskellorls.LsColor.Type,
  )
where

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Haskellorls.LsColor.Type

colorIndicatorsFrom :: [T.Text] -> LsColorDict
colorIndicatorsFrom = LsColorDict . M.fromList . Maybe.mapMaybe (makeKeyValuePair Monad.>=> makeFilenamePair)

makeFilenamePair :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
makeFilenamePair (ptn, esc) = filenamePattern ptn >>= (\ext -> Just (ext, esc))

filenamePattern :: T.Text -> Maybe T.Text
filenamePattern t
  | isPrefixWild t = Just . T.toUpper $ T.drop 1 t
  | otherwise = Nothing

parametorsFrom :: [T.Text] -> LsColorDict
parametorsFrom = LsColorDict . M.fromList . Maybe.mapMaybe (makeKeyValuePair Monad.>=> makeParametorPair)

makeParametorPair :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
makeParametorPair pair@(ptn, _)
  | isPrefixWild ptn = Nothing
  | otherwise = Just pair

isPrefixWild :: T.Text -> Bool
isPrefixWild = Maybe.maybe False (\p -> fst p == '*') . T.uncons

makeKeyValuePair :: T.Text -> Maybe (T.Text, T.Text)
makeKeyValuePair s = case pairs of
  [k, v] -> Just (k, v)
  _ -> Nothing
  where
    pairs = T.split (== '=') s
