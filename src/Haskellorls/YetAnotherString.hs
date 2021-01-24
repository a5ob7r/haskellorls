{-# LANGUAGE FlexibleInstances #-}

module Haskellorls.YetAnotherString
  ( YetAnotherString (..),
    Char,
    String,
    WrapedString (..),
  )
where

class YetAnotherString a where
  yaShow :: a -> String
  yaShow' :: a -> String
  yaLength :: a -> Int
  yaLength = length . yaShow

instance YetAnotherString Char where
  yaShow c = [c]
  yaShow' c = [c]

instance YetAnotherString String where
  yaShow = id
  yaShow' = id

data WrapedString = WrapedString
  { wrappedStringPrefix :: String,
    wrappedStringMain :: String,
    wrappedStringSuffix :: String
  }

instance YetAnotherString WrapedString where
  yaShow = wrappedStringMain
  yaShow' wStr = concatMap (\f -> f wStr) fs
    where
      fs =
        [ wrappedStringPrefix,
          wrappedStringMain,
          wrappedStringSuffix
        ]

instance YetAnotherString [WrapedString] where
  yaShow = concatMap yaShow
  yaShow' = concatMap yaShow'
