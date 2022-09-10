module Haskellorls.Config.Size
  ( BlockSize (..),
    BlockSizeMod (..),
    SizeNumberScale (..),
    SizeUnitScale (..),
  )
where

import Haskellorls.Humanize.FileSize

newtype SizeNumberScale a = SizeNumberScale {unSizeNumberScale :: Scale a}

newtype SizeUnitScale a = SizeUnitScale {unSizeUnitScale :: Scale a}

-- | A modifier for formats of block size.
data BlockSizeMod a
  = -- | With thousands separators.
    WithSeps a
  | -- | Just a block size, no any modification.
    NoMod a
  deriving (Show)

data BlockSize
  = HumanReadableBI
  | HumanReadableSI
  | KiloKibi
  | MegaKibi
  | GigaKibi
  | TeraKibi
  | PetaKibi
  | ExaKibi
  | ZettaKibi
  | YottaKibi
  | KiloKibii
  | MegaKibii
  | GigaKibii
  | TeraKibii
  | PetaKibii
  | ExaKibii
  | ZettaKibii
  | YottaKibii
  | KiloSi
  | MegaSi
  | GigaSi
  | TeraSi
  | PetaSi
  | ExaSi
  | ZettaSi
  | YottaSi
  | BlockSize Int
  deriving (Show)
