module Haskellorls.Formatter.Indicator (mkIndicatorPrinter) where

import qualified Data.Text as T
import Haskellorls.Class (Dictionary (lookup), from)
import qualified Haskellorls.Config as Config
import qualified Haskellorls.Config.Format as Format
import Haskellorls.Config.Indicator
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import qualified Haskellorls.NodeInfo as Node
import Prelude hiding (lookup)

newtype Indicator = Indicator Char

instance Dictionary Node.NodeInfo Indicator Config.Config where
  lookup node config = case Node.nodeType node of
    Node.SymbolicLink -> case Config.indicatorStyle config of
      _ | Format.LONG <- Config.format config -> Nothing
      IndicatorFiletype -> Just $ Indicator '@'
      IndicatorClassify -> Just $ Indicator '@'
      _ -> Nothing
    Node.NamedPipe -> case Config.indicatorStyle config of
      IndicatorFiletype -> Just $ Indicator '|'
      IndicatorClassify -> Just $ Indicator '|'
      _ -> Nothing
    Node.Socket -> case Config.indicatorStyle config of
      IndicatorFiletype -> Just $ Indicator '='
      IndicatorClassify -> Just $ Indicator '='
      _ -> Nothing
    Node.DoorsDevise -> case Config.indicatorStyle config of
      IndicatorFiletype -> Just $ Indicator '>'
      IndicatorClassify -> Just $ Indicator '>'
      _ -> Nothing
    Node.Executable -> case Config.indicatorStyle config of
      IndicatorClassify -> Just $ Indicator '*'
      _ -> Nothing
    nType
      | Node.isDirectory nType -> case Config.indicatorStyle config of
          IndicatorNone -> Nothing
          _ -> Just $ Indicator '/'
      | otherwise -> Nothing

mkIndicatorPrinter :: Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
mkIndicatorPrinter config =
  let f node = case Node.getNodeLinkInfo node of
        Just (Right _) | Format.LONG <- Config.format config -> Node.toFileInfo node
        _ -> node
   in \node -> maybe [] (\(Indicator c) -> [Attr.Other . from @T.Text $ T.singleton c]) $ f node `lookup` config
