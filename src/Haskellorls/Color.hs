module Haskellorls.Color where

import Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import Data.List.Extra (tails)
import Data.List.Split (endBy, splitOn)
import Data.Maybe
import System.Environment (getEnv)

type FilenamePtnMap = Map.Map String String

{-| Colorize String with ansii escape sequence.
-}
colorize :: String -> String -> String
colorize esc str = "\^[[" ++ esc ++ "m" ++ str ++ "\^[[m"

{-| Lookup ascii escape sequence. At first, lookup with a query as it is. If
    fails to lookup, change a query to the extension and re lookup.
-}
lookupEscSec :: FilenamePtnMap -> String -> String
lookupEscSec ptnMap = f . mapMaybe (\q -> Map.lookup q ptnMap) . reverse . tails . toUppers
  where f [] = ""
        f xs = head xs

colorIndicators :: IO FilenamePtnMap
colorIndicators = do
  envLSCOLORS <- getLSCOLORS
  return . Map.fromList . filenameEscSecs . concat . map (maybeToList . makePatternEscapePair) . endBy ":" $ envLSCOLORS

filenameEscSecs :: [(String, String)] -> [(String, String)]
filenameEscSecs =  concat . map (maybeToList . filenamePtnEscSec)

filenamePtnEscSec :: (String, String) -> Maybe (String, String)
filenamePtnEscSec (ptn, esc) = filenamePattern ptn >>= (\ext -> Just (ext, esc))

filenamePattern :: String -> Maybe String
filenamePattern str = f str >>= g
  where f s = if (length s) >= 1
                 then Just s
                 else Nothing
        g s = if (head s) == '*'
                 then Just . toUppers . drop 1 $ s
                 else Nothing

makePatternEscapePair :: String -> Maybe (String, String)
makePatternEscapePair s = if (length pairs) == 2
                              then Just (head pairs, last pairs)
                              else Nothing
  where pairs = splitOn "=" s

getLSCOLORS :: IO String
getLSCOLORS = getEnv "LS_COLORS"

toUppers :: String -> String
toUppers = map toUpper
