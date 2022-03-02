-- K : K, none
-- k : K, none
-- KiB : K, kibi
-- kiB : K, kibi
-- KB : K, si
-- kB : K, si
module Haskellorls.Size.Type
  ( SizeNumberScale (..),
    SizeUnitScale (..),
    Scale (..),
    BaseScale (..),
    ScaleSuffix (..),
    BlockSize (..),
  )
where

newtype SizeNumberScale = SizeNumberScale {unSizeNumberScale :: BaseScale}

newtype SizeUnitScale = SizeUnitScale {unSizeUnitScale :: BaseScale}

data Scale
  = NoScale
  | Scale Int

data BaseScale
  = BYTE
  | KILO
  | MEGA
  | GIGA
  | TERA
  | PETA
  | EXA
  | ZETTA
  | YOTTA

data ScaleSuffix
  = NONE
  | KIBI
  | KIBII
  | SI

data BlockSize
  = DefaultSize
  | HumanReadable
  | BlockSize
      { scale :: Scale,
        baseScale :: BaseScale,
        scaleSuffix :: ScaleSuffix
      }
