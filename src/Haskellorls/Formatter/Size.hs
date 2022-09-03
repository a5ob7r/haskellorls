-- |
-- = Format for @BLOCK_SIZE@, @LS_BLOCK_SIZE@ and @--block-size=SIZE@
--
-- * Any positive decimal, octal or hexadecimal number, these allow any
-- character sequence as suffix. And only octals allow any number of @0@ as
-- prefix.
--
--     These are valid.
--
--     * 1
--     * 02
--     * 0x3
--     * 00002000
--     * 3K
--     * 03foo
--     * 0x4Kfoo
--
--     But these are invalid.
--
--     * 0
--     * 0000
--     * 00bar
--     * -1
--     * -10bar
--     * 00x4
--     * 0xx2
--
-- * Binary prefixes or metric prefixes, but these allow any character sequence
-- as suffix.
--
--     These are all of valid prefixes, even if the first character is
--     lowercase.
--
--     * K
--     * KiB
--     * KB
--     * M
--     * MiB
--     * MB
--     * G
--     * GiB
--     * GB
--     * T
--     * TiB
--     * TB
--     * P
--     * PiB
--     * PB
--     * E
--     * EiB
--     * EB
--     * Z
--     * ZiB
--     * ZB
--     * Y
--     * YiB
--     * YB
--
--     These are valid.
--
--     * K
--     * miB
--     * gfoo
--     * TiBfoo
--     * pBfoo
--
-- * Constants for human readable.
--
--     * h
--     * hu
--     * hum
--     * huma
--     * human
--
-- = Scale size parameter
--
-- == How to determine the parameter
--
-- The scale size parameter is determined as below.
--
-- 1. Determined dymanically for autoscale if @-h@ / @--human-readable@ or
-- @--si@ are passed.
--
-- 2. Use @--block-size=SIZE@ option value if it is passed.
--
-- 3. Use @1024@ as block size if @-k@ / @--kibibyte@ is passed. This is
-- applied for block size only.
--
-- 4. Use @LS_BLOCK_SIZE@ environment variables as the scale size if it is
-- defined.
--
-- 5. Use @BLOCK_SIZE@ environment variables as the scale size if it is
-- defined.
--
-- 6. Use default value if does not match above any condition.
--
-- == How to interpret
--
-- The scale size parameter is interpreted as below.
--
-- 1. The scale size is changed dynamically for human readable according to
-- each file sizes if the parameter is one of below.
--
--     * h
--     * hu
--     * hum
--     * huma
--     * human
--
-- 2. The scale size is @n@ if the determined block size @n@ is just a decimal,
-- octal or hexadecimal number.
--
-- 3. The scale size is @n@ * @scale@ if the determined block size is
-- constructed with just a decimal, octal or hexadecimal number @n@ and a
-- binary or a metric prefix @scale@ and any string.
--
-- 4. The scale size is @scale@ if the determined block size is just a binary
-- or a metric prefix @scale@ and any string.
--
-- 5. The scale size must be a default size in other cases.
--
-- = Binary prefix or Metric prefix
--
-- These rules are for both file size and block size.
--
-- 1. Append a binary prefix as a size suffix if @--si@.
--
--     In this case, @K@ must be lowercase. This is for compatibility with ls
--     (GNU coreutils) 9.1.
--
-- 2. Append a binary prefix as a size suffix if @-h@ or @--human-readable@.
--
-- 3. Append a the value as a size suffix if the determined block size is just
-- a binary prefix or a metric suffix.
--
--     In this case, @KB@ must be @kB@ when appends it as a size suffix. This
--     is for compatibility with ls (GNU coreutils) 9.1.
--
-- 4. Nothing append a prefix as a size suffix in other cases.
module Haskellorls.Formatter.Size
  ( toTotalBlockSize,
    fileBlockSize,
    normalColoredFileBlockSize,
    coloredFileBlockSize,
    fileSize,
    normalColoredFileSize,
    coloredFileSize,
  )
where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Haskellorls.Class
import qualified Haskellorls.Config as Config
import Haskellorls.Config.DeviceNumber
import Haskellorls.Config.Size
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.WrappedText as WT
import Haskellorls.Humanize.FileSize
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types
import Prelude hiding (lookup)

data FileSizeComponent = FileSizeComponent
  { fileSizeRawNumber :: Types.FileOffset,
    fileSizeNumber :: T.Text,
    fileSizeUnit :: T.Text,
    fileSizeKibi :: Bool
  }

toWrappedText :: FileSizeComponent -> [Attr.Attribute WT.WrappedText]
toWrappedText FileSizeComponent {..} =
  [ Attr.Other $ from fileSizeNumber,
    Attr.Other $ from fileSizeUnit
  ]

toTotalBlockSize :: Config.Config -> [Types.FileOffset] -> [Attr.Attribute WT.WrappedText]
toTotalBlockSize config = toWrappedText . toTotalBlockSize' config

toTotalBlockSize' :: Config.Config -> [Types.FileOffset] -> FileSizeComponent
toTotalBlockSize' config = fileSize' config size . foldl' (\acc o -> acc + toBlockSize o) 0
  where
    size = case Config.blockSize config of
      DefaultSize
        | Config.si config -> HumanReadable
        | otherwise -> BlockSize 1024
      sz -> sz

fileBlockSize :: Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
fileBlockSize config node = case Node.fileSize node of
  Nothing -> [Attr.Missing $ from @T.Text "?"]
  Just _ -> toTotalBlockSize config [fileBlockSizeOf node]

-- | A node block size formatter for the @no@ parameter of @LS_COLORS@.
normalColoredFileBlockSize :: Color.LsColors -> Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredFileBlockSize lscolors config node = case Node.fileSize node of
  Nothing -> [Attr.Missing $ from @T.Text "?"]
  Just _ ->
    let FileSizeComponent {..} = toTotalBlockSize' config [fileBlockSizeOf node]
     in [Attr.Other $ WT.wrap lscolors Color.normal (fileSizeNumber <> fileSizeUnit)]

coloredFileBlockSize :: Color.LsColors -> Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredFileBlockSize lscolors config node = case Node.fileSize node of
  Nothing -> [Attr.Missing $ from @T.Text "?"]
  Just _ -> coloredFileSize' lscolors $ toTotalBlockSize' config [fileBlockSizeOf node]

-- | Calculate a block size of a file.
--
-- prop> (toBlockSize n) `mod` (1024 * 4) == 0
toBlockSize :: Integral a => a -> a
toBlockSize i
  | i < 0 = negate . toBlockSize $ negate i
  | otherwise = i `ceilingBy` (1024 * 4)

-- | Ceil @i@ to the value which is multiple of @n@, and is equal to @i@ or is
-- greater than @i@.
--
-- prop> (i `ceilingBy` d) `mod` d == 0
--
-- prop> (i `ceilingBy` d) >= i
--
-- >>> 1 `ceilingBy` 1
-- 1
--
-- >>> 1 `ceilingBy` 2
-- 2
--
-- >>> 2 `ceilingBy` 2
-- 2
--
-- >>> 3 `ceilingBy` 2
-- 4
ceilingBy :: Integral a => a -> a -> a
ceilingBy i d = (i `ceilingDiv` d) * d

-- | 'div' @i@ by @d@, but add @1@ to the @quosient@ if the @remainder@ is not
-- @0@.
--
-- prop> i `div` d <= i `ceilingDiv` d
ceilingDiv :: Integral a => a -> a -> a
ceilingDiv i d = q + abs (signum r)
  where
    (q, r) = i `divMod` d

fileSize :: (Int, Int) -> Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
fileSize widths config node = case Node.nodeType node of
  Just Node.BlockDevise -> deviceNumbers widths Nothing config node
  Just Node.CharDevise -> deviceNumbers widths Nothing config node
  _ -> maybe [Attr.Missing $ from @T.Text "?"] (toWrappedText . fileSize' config (Config.fileSize config)) $ Node.fileSize node

fileSize' :: Config.Config -> BlockSize -> Types.FileOffset -> FileSizeComponent
fileSize' config size offset = case size of
  _ | Config.si config -> humanize config offset
  DefaultSize -> fileSize' config (BlockSize 1) offset
  HumanReadable -> humanize config offset
  KiloKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (1 :: Int)) "K" True
  KiloKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (1 :: Int)) "KiB" True
  -- NOTE: This lowercased "k" is intended for compatibility with GNU ls.
  KiloSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (1 :: Int)) "kB" True
  MegaKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (2 :: Int)) "M" True
  MegaKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (2 :: Int)) "MiB" True
  MegaSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (2 :: Int)) "MB" True
  GigaKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (3 :: Int)) "G" True
  GigaKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (3 :: Int)) "GiB" True
  GigaSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (3 :: Int)) "GB" True
  TeraKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (4 :: Int)) "T" True
  TeraKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (4 :: Int)) "TiB" True
  TeraSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (4 :: Int)) "TB" True
  PetaKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (5 :: Int)) "P" True
  PetaKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (5 :: Int)) "PiB" True
  PetaSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (5 :: Int)) "PB" True
  ExaKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (6 :: Int)) "E" True
  ExaKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (6 :: Int)) "EiB" True
  ExaSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (6 :: Int)) "EB" True
  ZettaKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (7 :: Int)) "Z" True
  ZettaKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (7 :: Int)) "ZiB" True
  ZettaSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (7 :: Int)) "ZB" True
  YottaKibi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (8 :: Int)) "Y" True
  YottaKibii -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1024 ^ (8 :: Int)) "YiB" True
  YottaSi -> FileSizeComponent offset (T.pack . show @Int . ceiling @Double $ fromIntegral offset / 1000 ^ (8 :: Int)) "YB" True
  BlockSize n -> FileSizeComponent offset (T.pack . show $ fromIntegral offset `ceilingDiv` n) "" True

-- | A node file size formatter for the @no@ parameter of the @LS_COLORS@.
normalColoredFileSize :: (Int, Int) -> Color.LsColors -> Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredFileSize widths lscolors config node = case Node.nodeType node of
  Just Node.BlockDevise -> deviceNumbers widths (Just lscolors) config node
  Just Node.CharDevise -> deviceNumbers widths (Just lscolors) config node
  _ -> case Node.fileSize node of
    Just fsize ->
      let FileSizeComponent {..} = fileSize' config (Config.fileSize config) fsize
       in [Attr.Other $ WT.wrap lscolors Color.normal (fileSizeNumber <> fileSizeUnit)]
    _ -> [Attr.Missing $ from @T.Text "?"]

coloredFileSize :: (Int, Int) -> Color.LsColors -> Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredFileSize widths lscolors config node = case Node.nodeType node of
  Just Node.BlockDevise -> deviceNumbers widths (Just lscolors) config node
  Just Node.CharDevise -> deviceNumbers widths (Just lscolors) config node
  _ -> case Node.fileSize node of
    Just fsize -> coloredFileSize' lscolors $ fileSize' config (Config.fileSize config) fsize
    _ -> [Attr.Missing $ from @T.Text "?"]

coloredFileSize' :: Color.LsColors -> FileSizeComponent -> [Attr.Attribute WT.WrappedText]
coloredFileSize' lscolors FileSizeComponent {..} =
  [ Attr.Other $ WT.wrap lscolors sizeSelector fileSizeNumber,
    Attr.Other $ WT.wrap lscolors unitSelector fileSizeUnit
  ]
  where
    sizeSelector = Color.lookup . SizeNumberScale $ unwrap bScale
    unitSelector = Color.lookup . SizeUnitScale $ unwrap bScale
    bScale
      | fileSizeKibi = humanizeBI fileSizeRawNumber
      | otherwise = humanizeSI fileSizeRawNumber
    unwrap = \case
      BI scaled -> scaled
      SI scaled -> scaled

deviceNumbers :: (Int, Int) -> Maybe Color.LsColors -> Config.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
deviceNumbers widths@(majorWidth, minorWidth) lscolors config node = case lscolors of
  Nothing ->
    Attr.Other
      <$> WT.justifyRight majorWidth ' ' [from major]
        <> delimiter
        <> WT.justifyRight minorWidth ' ' [from minor]
  Just lc -> case Config.colorize config of
    Just True ->
      Attr.Other
        <$> WT.justifyRight majorWidth ' ' [WT.wrap lc (maybe (const Nothing) lookup majorID) major]
          <> delimiter
          <> WT.justifyRight minorWidth ' ' [WT.wrap lc (maybe (const Nothing) lookup minorID) minor]
    Just False ->
      Attr.Other
        <$> WT.justifyRight majorWidth ' ' [WT.wrap lc Color.normal major]
          <> delimiter
          <> WT.justifyRight minorWidth ' ' [WT.wrap lc Color.normal minor]
    _ -> deviceNumbers widths Nothing config node
  where
    delimiter = [from @T.Text ", "]
    majorID = from <$> Node.specialDeviceID node
    minorID = from <$> Node.specialDeviceID node
    major = maybe "?" (from @MajorID) majorID
    minor = maybe "?" (from @MinorID) minorID

humanize :: Config.Config -> Types.FileOffset -> FileSizeComponent
humanize config size = case size' of
  BI (NoScale (ISize n)) -> FileSizeComponent size (T.pack $ show n) "" True
  BI (NoScale (DSize d)) -> FileSizeComponent size (T.pack $ show d) "" True
  BI (Kilo (ISize n)) -> FileSizeComponent size (T.pack $ show n) "K" True
  BI (Kilo (DSize d)) -> FileSizeComponent size (T.pack $ show d) "K" True
  BI (Mega (ISize n)) -> FileSizeComponent size (T.pack $ show n) "M" True
  BI (Mega (DSize d)) -> FileSizeComponent size (T.pack $ show d) "M" True
  BI (Giga (ISize n)) -> FileSizeComponent size (T.pack $ show n) "G" True
  BI (Giga (DSize d)) -> FileSizeComponent size (T.pack $ show d) "G" True
  BI (Tera (ISize n)) -> FileSizeComponent size (T.pack $ show n) "T" True
  BI (Tera (DSize d)) -> FileSizeComponent size (T.pack $ show d) "T" True
  BI (Peta (ISize n)) -> FileSizeComponent size (T.pack $ show n) "P" True
  BI (Peta (DSize d)) -> FileSizeComponent size (T.pack $ show d) "P" True
  BI (Exa (ISize n)) -> FileSizeComponent size (T.pack $ show n) "E" True
  BI (Exa (DSize d)) -> FileSizeComponent size (T.pack $ show d) "E" True
  BI (Zetta (ISize n)) -> FileSizeComponent size (T.pack $ show n) "Z" True
  BI (Zetta (DSize d)) -> FileSizeComponent size (T.pack $ show d) "Z" True
  BI (Yotta (ISize n)) -> FileSizeComponent size (T.pack $ show n) "Y" True
  BI (Yotta (DSize d)) -> FileSizeComponent size (T.pack $ show d) "Y" True
  SI (NoScale (ISize n)) -> FileSizeComponent size (T.pack $ show n) "" True
  SI (NoScale (DSize d)) -> FileSizeComponent size (T.pack $ show d) "" True
  -- NOTE: Both or lowercased "k" are intended for compatibility with GNU ls.
  SI (Kilo (ISize n)) -> FileSizeComponent size (T.pack $ show n) "k" True
  SI (Kilo (DSize d)) -> FileSizeComponent size (T.pack $ show d) "k" True
  SI (Mega (ISize n)) -> FileSizeComponent size (T.pack $ show n) "M" True
  SI (Mega (DSize d)) -> FileSizeComponent size (T.pack $ show d) "M" True
  SI (Giga (ISize n)) -> FileSizeComponent size (T.pack $ show n) "G" True
  SI (Giga (DSize d)) -> FileSizeComponent size (T.pack $ show d) "G" True
  SI (Tera (ISize n)) -> FileSizeComponent size (T.pack $ show n) "T" True
  SI (Tera (DSize d)) -> FileSizeComponent size (T.pack $ show d) "T" True
  SI (Peta (ISize n)) -> FileSizeComponent size (T.pack $ show n) "P" True
  SI (Peta (DSize d)) -> FileSizeComponent size (T.pack $ show d) "P" True
  SI (Exa (ISize n)) -> FileSizeComponent size (T.pack $ show n) "E" True
  SI (Exa (DSize d)) -> FileSizeComponent size (T.pack $ show d) "E" True
  SI (Zetta (ISize n)) -> FileSizeComponent size (T.pack $ show n) "Z" True
  SI (Zetta (DSize d)) -> FileSizeComponent size (T.pack $ show d) "Z" True
  SI (Yotta (ISize n)) -> FileSizeComponent size (T.pack $ show n) "Y" True
  SI (Yotta (DSize d)) -> FileSizeComponent size (T.pack $ show d) "Y" True
  where
    size' =
      if Config.si config
        then humanizeSI size
        else humanizeBI size

-- | Treat symbolic link block size as zero.
fileBlockSizeOf :: Node.NodeInfo -> Types.FileOffset
fileBlockSizeOf node = maybe (fromMaybe 0 $ Node.fileSize node) (const 0) $ Node.getNodeLinkInfo node
