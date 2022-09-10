{-# LANGUAGE ForeignFunctionInterface #-}

#include <locale.h>
#include <limits.h>

module System.Locale.LocaleConv
  ( Lconv,
    localeConv,
    decimalPoint,
    thousandsSep,
    grouping,
    charMax,
  )
where

import Data.Char (ord)
import Foreign (Ptr, peekByteOff)
import Foreign.C.String (peekCString)
import System.IO.Unsafe (unsafePerformIO)

data {-# CTYPE "struct lconv" #-} CLconv

foreign import ccall "locale.h localeconv" c_localeconv :: IO (Ptr CLconv)

newtype Lconv = Lconv (Ptr CLconv)

localeConv :: IO Lconv
localeConv = Lconv <$> c_localeconv

-- TODO: There are many missing getters for @lconv@.

decimalPoint :: Lconv -> String
decimalPoint (Lconv ptr) = unsafePerformIO $ (#peek struct lconv, decimal_point) ptr >>= peekCString

thousandsSep :: Lconv -> String
thousandsSep (Lconv ptr) = unsafePerformIO $ (#peek struct lconv, thousands_sep) ptr >>= peekCString

grouping :: Lconv -> [Int]
grouping (Lconv ptr) = unsafePerformIO $ (#peek struct lconv, grouping) ptr >>= fmap (ord <$>) . peekCString

-- | @CHAR_MAX@ in the system. This value in 'grouping' has a special meaning.
-- Please see a description about @grouping@, which is a field of @lconv@, in
-- @locale(7)@.
charMax :: Int
charMax = #const CHAR_MAX
