module Haskellorls.Formatter.Indicator (mkIndicatorPrinter) where

import Data.Text qualified as T
import Haskellorls.Class (Dictionary (lookup))
import Haskellorls.Config qualified as Config
import Haskellorls.Config.Format qualified as Format
import Haskellorls.Config.Indicator
import Haskellorls.Formatter.Attribute qualified as Attr
import Haskellorls.Formatter.WrappedText qualified as WT
import Haskellorls.NodeInfo qualified as Node
import Witch (From (..))
import Prelude hiding (lookup)

newtype Indicator = Indicator Char

instance Dictionary Node.NodeInfo Indicator Config.Config where
  lookup node config = case Node.nodeType node of
    Nothing -> Nothing
    Just Node.SymbolicLink -> case Config.indicatorStyle config of
      _ | Format.LONG <- Config.format config -> Nothing
      IndicatorFiletype -> Just $ Indicator '@'
      IndicatorClassify -> Just $ Indicator '@'
      _ -> Nothing
    Just Node.NamedPipe -> case Config.indicatorStyle config of
      IndicatorFiletype -> Just $ Indicator '|'
      IndicatorClassify -> Just $ Indicator '|'
      _ -> Nothing
    Just Node.Socket -> case Config.indicatorStyle config of
      IndicatorFiletype -> Just $ Indicator '='
      IndicatorClassify -> Just $ Indicator '='
      _ -> Nothing
    Just Node.DoorsDevise -> case Config.indicatorStyle config of
      IndicatorFiletype -> Just $ Indicator '>'
      IndicatorClassify -> Just $ Indicator '>'
      _ -> Nothing
    Just Node.Executable -> case Config.indicatorStyle config of
      IndicatorClassify -> Just $ Indicator '*'
      _ -> Nothing
    Just nType
      | Node.isDirectory nType -> case Config.indicatorStyle config of
          IndicatorNone -> Nothing
          _ -> Just $ Indicator '/'
      | otherwise -> Nothing

mkIndicatorPrinter :: Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
mkIndicatorPrinter config =
  let f node = case Node.getNodeLinkInfo node of
        Just (Right _) | Format.LONG <- Config.format config -> Node.dereference node
        _ -> node
   in \node -> maybe [] (\(Indicator c) -> [Attr.Other . from @T.Text $ T.singleton c]) $ f node `lookup` config
