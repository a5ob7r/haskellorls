-- K : K, none
-- k : K, none
-- KiB : K, kibi
-- kiB : K, kibi
-- KB : K, si
-- kB : K, si
module Haskellorls.Size.Type
  ( Scale (..),
    BaseScale (..),
    ScaleSuffix (..),
    BlockSize (..),
  )
where

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
  = HumanReadable
  | BlockSize
      { scale :: Scale,
        baseScale :: BaseScale,
        scaleSuffix :: ScaleSuffix
      }
