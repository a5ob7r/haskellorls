{-# LANGUAGE PackageImports #-}

module Prelude
  ( module Prelude,
    foldl',
  )
where

#if MIN_VERSION_base(4,20,0)
import "base" Prelude hiding (foldl')
#else
import "base" Prelude
#endif

import Data.Foldable (foldl')
