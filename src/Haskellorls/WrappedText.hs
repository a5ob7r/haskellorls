module Haskellorls.WrappedText
  ( WrappedText (..),
    wtLength,
    wtLengthForDisplay,
    render,
    toWrappedText,
    toWrappedTextSingleton,
    toList,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Utils as Utils

data WrappedText = WrappedText
  { wtPrefix :: T.Text,
    wtWord :: T.Text,
    wtSuffix :: T.Text
  }

wtLength :: WrappedText -> Int
wtLength = T.length . wtWord

wtLengthForDisplay :: WrappedText -> Int
wtLengthForDisplay = Utils.textLengthForDisplay . wtWord

render :: WrappedText -> T.Text
render wt = wtPrefix wt <> wtWord wt <> wtSuffix wt

toWrappedText :: T.Text -> WrappedText
toWrappedText t = WrappedText "" t ""

toWrappedTextSingleton :: T.Text -> [WrappedText]
toWrappedTextSingleton t = [toWrappedText t]

toList :: WrappedText -> [T.Text]
toList wt = map (\f -> f wt) [wtPrefix, wtWord, wtSuffix]
