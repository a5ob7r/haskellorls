{-# LANGUAGE OverloadedStrings #-}

module Haskellorls.WrappedText
  ( WrappedText (..),
    wtLength,
    render,
    toWrappedText,
    toWrappedTextSingleton,
  )
where

import qualified Data.Text as T

data WrappedText = WrappedText
  { wtPrefix :: T.Text,
    wtWord :: T.Text,
    wtSuffix :: T.Text
  }

wtLength :: WrappedText -> Int
wtLength = T.length . wtWord

render :: WrappedText -> T.Text
render wt = wtPrefix wt <> wtWord wt <> wtSuffix wt

toWrappedText :: T.Text -> WrappedText
toWrappedText t = WrappedText "" t ""

toWrappedTextSingleton :: T.Text -> [WrappedText]
toWrappedTextSingleton t = [toWrappedText t]
