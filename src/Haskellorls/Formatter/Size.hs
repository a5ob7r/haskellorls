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
--     * human-
--     * human-r
--     * human-re
--     * human-rea
--     * human-read
--     * human-reada
--     * human-readab
--     * human-readabl
--     * human-readable
--     * s
--     * si
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
--     * human-
--     * human-r
--     * human-re
--     * human-rea
--     * human-read
--     * human-reada
--     * human-readab
--     * human-readabl
--     * human-readable
--     * s
--     * si
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
import Haskellorls.Class (lookup)
import qualified Haskellorls.Config as Config
import Haskellorls.Config.DeviceNumber
import Haskellorls.Config.Size
import qualified Haskellorls.Formatter.Attribute as Attr
import qualified Haskellorls.Formatter.Number as Number
import qualified Haskellorls.Formatter.WrappedText as WT
import Haskellorls.Humanize.FileSize
import qualified Haskellorls.LsColor as Color
import qualified Haskellorls.NodeInfo as Node
import qualified System.Posix.Types as Types
import Witch (from)
import Prelude hiding (lookup)

data FileSizeComponent = FileSizeComponent
  { fileSizeRawNumber :: Types.FileOffset,
    fileSizeNumber :: T.Text,
    fileSizeUnit :: T.Text,
    fileSizeScale :: Scale () ()
  }

toWrappedText :: FileSizeComponent -> [Attr.Attribute WT.WrappedText]
toWrappedText FileSizeComponent {..} =
  [ Attr.Other $ from fileSizeNumber,
    Attr.Other $ from fileSizeUnit
  ]

toTotalBlockSize :: Config.Config -> Number.Config -> [Types.FileOffset] -> [Attr.Attribute WT.WrappedText]
toTotalBlockSize config nconfig = toWrappedText . toTotalBlockSize' config nconfig

toTotalBlockSize' :: Config.Config -> Number.Config -> [Types.FileOffset] -> FileSizeComponent
toTotalBlockSize' config nconfig = fileSize' nconfig (Config.blockSize config) . foldl' (\acc o -> acc + toBlockSize o) 0

fileBlockSize :: Config.Config -> Number.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
fileBlockSize config nconfig node = case Node.fileSize node of
  Nothing -> [Attr.Missing $ from @T.Text "?"]
  Just _ -> toTotalBlockSize config nconfig [fileBlockSizeOf node]

-- | A node block size formatter for the @no@ parameter of @LS_COLORS@.
normalColoredFileBlockSize :: Color.LsColors -> Config.Config -> Number.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredFileBlockSize lscolors config nconfig node = case Node.fileSize node of
  Nothing -> [Attr.Missing $ from @T.Text "?"]
  Just _ ->
    let FileSizeComponent {..} = toTotalBlockSize' config nconfig [fileBlockSizeOf node]
     in [Attr.Other $ WT.wrap lscolors Color.normal (fileSizeNumber <> fileSizeUnit)]

coloredFileBlockSize :: Color.LsColors -> Config.Config -> Number.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredFileBlockSize lscolors config nconfig node = case Node.fileSize node of
  Nothing -> [Attr.Missing $ from @T.Text "?"]
  Just _ -> coloredFileSize' lscolors $ toTotalBlockSize' config nconfig [fileBlockSizeOf node]

-- | Calculate a block size of a file.
--
-- prop> (toBlockSize n) `mod` (1024 * 4) == 0
toBlockSize :: (Integral a) => a -> a
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
ceilingBy :: (Integral a) => a -> a -> a
ceilingBy i d = (i `ceilingDiv` d) * d

-- | 'div' @i@ by @d@, but add @1@ to the @quosient@ if the @remainder@ is not
-- @0@.
--
-- prop> i `div` d <= i `ceilingDiv` d
ceilingDiv :: (Integral a) => a -> a -> a
ceilingDiv i d = q + abs (signum r)
  where
    (q, r) = i `divMod` d

fileSize :: (Int, Int) -> Config.Config -> Number.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
fileSize widths config nconfig node = case Node.nodeType node of
  Just Node.BlockDevise -> deviceNumbers widths Nothing config node
  Just Node.CharDevise -> deviceNumbers widths Nothing config node
  _ -> maybe [Attr.Missing $ from @T.Text "?"] (toWrappedText . fileSize' nconfig (Config.fileSize config)) $ Node.fileSize node

fileSize' :: Number.Config -> BlockSizeMod BlockSize -> Types.FileOffset -> FileSizeComponent
fileSize' nconfig blocksize offset =
  let (bsize, toText) = case blocksize of
        WithSeps sz ->
          let format = \case
                ISize i -> Number.formatI nconfig i
                DSize d -> Number.formatF nconfig d
           in (sz, T.pack . format)
        NoMod sz ->
          let format = \case
                ISize i -> show i
                DSize d -> show d
           in (sz, T.pack . format)
   in case bsize of
        HumanReadableSI -> case humanizeSI offset of
          NoScale size -> FileSizeComponent offset (toText size) "" (NoScale ())
          -- NOTE: This lowercased @k@ is intended for compatibility with GNU ls.
          Kilo size -> FileSizeComponent offset (toText size) "k" (Kilo ())
          Mega size -> FileSizeComponent offset (toText size) "M" (Mega ())
          Giga size -> FileSizeComponent offset (toText size) "G" (Giga ())
          Tera size -> FileSizeComponent offset (toText size) "T" (Tera ())
          Peta size -> FileSizeComponent offset (toText size) "P" (Peta ())
          Exa size -> FileSizeComponent offset (toText size) "E" (Exa ())
          Zetta size -> FileSizeComponent offset (toText size) "Z" (Zetta ())
          Yotta size -> FileSizeComponent offset (toText size) "Y" (Yotta ())
        HumanReadableBI -> case humanizeBI offset of
          NoScale size -> FileSizeComponent offset (toText size) "" (NoScale ())
          Kilo size -> FileSizeComponent offset (toText size) "K" (Kilo ())
          Mega size -> FileSizeComponent offset (toText size) "M" (Mega ())
          Giga size -> FileSizeComponent offset (toText size) "G" (Giga ())
          Tera size -> FileSizeComponent offset (toText size) "T" (Tera ())
          Peta size -> FileSizeComponent offset (toText size) "P" (Peta ())
          Exa size -> FileSizeComponent offset (toText size) "E" (Exa ())
          Zetta size -> FileSizeComponent offset (toText size) "Z" (Zetta ())
          Yotta size -> FileSizeComponent offset (toText size) "Y" (Yotta ())
        KiloKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (1 :: Int)) "K" (Kilo ())
        KiloKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (1 :: Int)) "KiB" (Kilo ())
        -- NOTE: This lowercased "k" is intended for compatibility with GNU ls.
        KiloSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (1 :: Int)) "kB" (Kilo ())
        MegaKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (2 :: Int)) "M" (Mega ())
        MegaKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (2 :: Int)) "MiB" (Mega ())
        MegaSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (2 :: Int)) "MB" (Mega ())
        GigaKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (3 :: Int)) "G" (Giga ())
        GigaKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (3 :: Int)) "GiB" (Giga ())
        GigaSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (3 :: Int)) "GB" (Giga ())
        TeraKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (4 :: Int)) "T" (Tera ())
        TeraKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (4 :: Int)) "TiB" (Tera ())
        TeraSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (4 :: Int)) "TB" (Tera ())
        PetaKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (5 :: Int)) "P" (Peta ())
        PetaKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (5 :: Int)) "PiB" (Peta ())
        PetaSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (5 :: Int)) "PB" (Peta ())
        ExaKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (6 :: Int)) "E" (Exa ())
        ExaKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (6 :: Int)) "EiB" (Exa ())
        ExaSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (6 :: Int)) "EB" (Exa ())
        ZettaKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (7 :: Int)) "Z" (Zetta ())
        ZettaKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (7 :: Int)) "ZiB" (Zetta ())
        ZettaSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (7 :: Int)) "ZB" (Zetta ())
        YottaKibi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (8 :: Int)) "Y" (Yotta ())
        YottaKibii -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1024 ^ (8 :: Int)) "YiB" (Yotta ())
        YottaSi -> FileSizeComponent offset (toText . ISize . ceiling @Double $ fromIntegral offset / 1000 ^ (8 :: Int)) "YB" (Yotta ())
        -- TODO: Should change the scale dynamically?
        BlockSize n -> FileSizeComponent offset (toText . ISize $ fromIntegral offset `ceilingDiv` n) "" (NoScale ())

-- | A node file size formatter for the @no@ parameter of the @LS_COLORS@.
normalColoredFileSize :: (Int, Int) -> Color.LsColors -> Config.Config -> Number.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
normalColoredFileSize widths lscolors config nconfig node = case Node.nodeType node of
  Just Node.BlockDevise -> deviceNumbers widths (Just lscolors) config node
  Just Node.CharDevise -> deviceNumbers widths (Just lscolors) config node
  _ -> case Node.fileSize node of
    Just fsize ->
      let FileSizeComponent {..} = fileSize' nconfig (Config.fileSize config) fsize
       in [Attr.Other $ WT.wrap lscolors Color.normal (fileSizeNumber <> fileSizeUnit)]
    _ -> [Attr.Missing $ from @T.Text "?"]

coloredFileSize :: (Int, Int) -> Color.LsColors -> Config.Config -> Number.Config -> Node.NodeInfo -> [Attr.Attribute WT.WrappedText]
coloredFileSize widths lscolors config nconfig node = case Node.nodeType node of
  Just Node.BlockDevise -> deviceNumbers widths (Just lscolors) config node
  Just Node.CharDevise -> deviceNumbers widths (Just lscolors) config node
  _ -> case Node.fileSize node of
    Just fsize -> coloredFileSize' lscolors $ fileSize' nconfig (Config.fileSize config) fsize
    _ -> [Attr.Missing $ from @T.Text "?"]

coloredFileSize' :: Color.LsColors -> FileSizeComponent -> [Attr.Attribute WT.WrappedText]
coloredFileSize' lscolors FileSizeComponent {..} =
  [ Attr.Other $ WT.wrap lscolors (Color.lookup $ SizeNumberScale fileSizeScale) fileSizeNumber,
    Attr.Other $ WT.wrap lscolors (Color.lookup $ SizeUnitScale fileSizeScale) fileSizeUnit
  ]

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

-- | Treat symbolic link block size as zero.
fileBlockSizeOf :: Node.NodeInfo -> Types.FileOffset
fileBlockSizeOf node = maybe (fromMaybe 0 $ Node.fileSize node) (const 0) $ Node.getNodeLinkInfo node
