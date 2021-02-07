{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- NOTE: When `--block-size` option is passed, file size format are `4K`,
-- `1111K` not `4.0K`, `1111.0K`

module Haskellorls.Size.Decorator
  ( fileSize,
    coloredFileSize,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Color as Color
import qualified Haskellorls.NodeInfo as Node
import qualified Haskellorls.Option as Option
import Haskellorls.Size.Type
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

data FileSizeComponent = FileSizeComponent
  { fileSizeRawNumber :: Types.FileOffset,
    fileSizeNumber :: T.Text,
    fileSizeUnit :: T.Text,
    fileSizeKibi :: Bool
  }

newtype UnitSize = UnitSize {getUnitSize :: Int}

fileSize :: Option.Option -> Node.NodeInfo -> [WT.WrappedText]
fileSize opt node =
  [ WT.toWrappedText $ fileSizeNumber component,
    WT.toWrappedText $ fileSizeUnit component
  ]
  where
    component = fileSize' opt node

-- FIXME: Show file size suffix when only passed file size unit (e.g. --block-size=K)
fileSize' :: Option.Option -> Node.NodeInfo -> FileSizeComponent
fileSize' opt node = case Option.blockSize opt of
  HumanReadable
    | Option.si opt -> siHumanReadableFileSize node
    | otherwise -> kibiHumanReadableFileSize node
  blockSize@BlockSize {..}
    | Option.humanReadable opt -> fileSize' opt {Option.blockSize = HumanReadable} node
    | otherwise -> FileSizeComponent {..}
    where
      fileSizeNumber = formatSize $ fromIntegral fileSizeRawNumber / fromIntegral (getUnitSize unitSize)
      fileSizeUnit = case scale of
        NoScale -> fileSizeUnitSelector scaleSuffix baseScale
        _ -> ""
      fileSizeRawNumber = fileSizeOf node
      unitSize = calcUnitSize blockSize
      fileSizeKibi = case scaleSuffix of
        SI -> False
        _ -> True

coloredFileSize :: Color.Config -> Option.Option -> Node.NodeInfo -> [WT.WrappedText]
coloredFileSize config opt node =
  [ Color.toWrappedText config sizeSelector fileSizeNumber,
    Color.toWrappedText config unitSelector fileSizeUnit
  ]
  where
    FileSizeComponent {..} = fileSize' opt node
    unitSelector = unitEscapeSequenceSelector bScale
    sizeSelector = sizeEscapeSequenceSelector bScale
    bScale
      | fileSizeKibi = detectKibiBlockSizeType fileSizeRawNumber
      | otherwise = detectSiBlockSizeType fileSizeRawNumber

kibiHumanReadableFileSize :: Node.NodeInfo -> FileSizeComponent
kibiHumanReadableFileSize node = FileSizeComponent {..}
  where
    fileSizeRawNumber = fileSizeOf node
    fileSizeNumber = kibiFileSizeAs baseScale fileSizeRawNumber
    fileSizeUnit = fileSizeUnitSelector KIBI baseScale
    fileSizeKibi = True
    baseScale = detectKibiBlockSizeType fileSizeRawNumber

siHumanReadableFileSize :: Node.NodeInfo -> FileSizeComponent
siHumanReadableFileSize node = FileSizeComponent {..}
  where
    fileSizeRawNumber = fileSizeOf node
    fileSizeNumber = siFileSizeAs baseScale fileSizeRawNumber
    fileSizeUnit = fileSizeUnitSelector KIBI baseScale
    fileSizeKibi = False
    baseScale = detectSiBlockSizeType fileSizeRawNumber

fileSizeOf :: Node.NodeInfo -> Types.FileOffset
fileSizeOf = Files.fileSize . Node.nodeInfoStatus

fileSizeUnitSelector :: ScaleSuffix -> BaseScale -> T.Text
fileSizeUnitSelector ss = case ss of
  NONE -> const ""
  KIBI -> kibiUnit
  KIBII -> kibiiUnit
  SI -> siUnit

calcUnitSize :: BlockSize -> UnitSize
calcUnitSize HumanReadable = UnitSize 1
calcUnitSize BlockSize {..} = UnitSize $ scale' * sizeGetter baseScale
  where
    scale' = case scale of
      NoScale -> 1
      Scale n -> n
    sizeGetter = case scaleSuffix of
      NONE -> const 1
      KIBI -> kibiSize
      KIBII -> kibiSize
      SI -> siSize

kibiSize :: (Num a) => BaseScale -> a
kibiSize bScale = case bScale of
  BYTE -> 1
  KILO -> sizeKi
  MEGA -> sizeMi
  GIGA -> sizeGi
  TERA -> sizeTi
  PETA -> sizePi
  EXA -> sizeEi
  ZETTA -> sizeZi
  YOTTA -> sizeYi

siSize :: (Num a) => BaseScale -> a
siSize bScale = case bScale of
  BYTE -> 1
  KILO -> sizeK
  MEGA -> sizeM
  GIGA -> sizeG
  TERA -> sizeT
  PETA -> sizeP
  EXA -> sizeE
  ZETTA -> sizeZ
  YOTTA -> sizeY

detectSiBlockSizeType :: Types.FileOffset -> BaseScale
detectSiBlockSizeType offset
  | offset < sizeK = BYTE
  | offset < sizeM = KILO
  | offset < sizeG = MEGA
  | offset < sizeT = GIGA
  | offset < sizeP = TERA
  | offset < sizeE = PETA
  | offset < sizeZ = EXA
  | offset < sizeY = ZETTA
  | otherwise = YOTTA

detectKibiBlockSizeType :: Types.FileOffset -> BaseScale
detectKibiBlockSizeType offset
  | offset < sizeKi = BYTE
  | offset < sizeMi = KILO
  | offset < sizeGi = MEGA
  | offset < sizeTi = GIGA
  | offset < sizePi = TERA
  | offset < sizeEi = PETA
  | offset < sizeZi = EXA
  | offset < sizeYi = ZETTA
  | otherwise = YOTTA

sizeEscapeSequenceSelector :: BaseScale -> Color.Config -> T.Text
sizeEscapeSequenceSelector bScale = case bScale of
  BYTE -> Color.fileSizeNumberEscapeSequence . Color.extensionColorConfig
  KILO -> Color.fileSizeNumberKiloEscapeSequence . Color.extensionColorConfig
  MEGA -> Color.fileSizeNumberMegaEscapeSequence . Color.extensionColorConfig
  GIGA -> Color.fileSizeNumberGigaEscapeSequence . Color.extensionColorConfig
  TERA -> Color.fileSizeNumberTeraEscapeSequence . Color.extensionColorConfig
  PETA -> Color.fileSizeNumberPetaEscapeSequence . Color.extensionColorConfig
  EXA -> Color.fileSizeNumberExaEscapeSequence . Color.extensionColorConfig
  ZETTA -> Color.fileSizeNumberZettaEscapeSequence . Color.extensionColorConfig
  YOTTA -> Color.fileSizeNumberYottaEscapeSequence . Color.extensionColorConfig

unitEscapeSequenceSelector :: BaseScale -> Color.Config -> T.Text
unitEscapeSequenceSelector bScale = case bScale of
  BYTE -> Color.fileSizeUnitBypeEscapeSequence . Color.extensionColorConfig
  KILO -> Color.fileSizeUnitKiloEscapeSequence . Color.extensionColorConfig
  MEGA -> Color.fileSizeUnitMegaEscapeSequence . Color.extensionColorConfig
  GIGA -> Color.fileSizeUnitGigaEscapeSequence . Color.extensionColorConfig
  TERA -> Color.fileSizeUnitTeraEscapeSequence . Color.extensionColorConfig
  PETA -> Color.fileSizeUnitPetaEscapeSequence . Color.extensionColorConfig
  EXA -> Color.fileSizeUnitExaEscapeSequence . Color.extensionColorConfig
  ZETTA -> Color.fileSizeUnitZettaEscapeSequence . Color.extensionColorConfig
  YOTTA -> Color.fileSizeUnitYottaEscapeSequence . Color.extensionColorConfig

kibiUnit :: BaseScale -> T.Text
kibiUnit bScale = case bScale of
  BYTE -> ""
  KILO -> "K"
  MEGA -> "M"
  GIGA -> "G"
  TERA -> "T"
  PETA -> "P"
  EXA -> "E"
  ZETTA -> "Z"
  YOTTA -> "Y"

kibiiUnit :: BaseScale -> T.Text
kibiiUnit bScale = case bScale of
  BYTE -> ""
  KILO -> "KiB"
  MEGA -> "MiB"
  GIGA -> "GiB"
  TERA -> "TiB"
  PETA -> "PiB"
  EXA -> "EiB"
  ZETTA -> "ZiB"
  YOTTA -> "YiB"

siUnit :: BaseScale -> T.Text
siUnit bScale = case bScale of
  BYTE -> ""
  KILO -> "KB"
  MEGA -> "MB"
  GIGA -> "GB"
  TERA -> "TB"
  PETA -> "PB"
  EXA -> "EB"
  ZETTA -> "ZB"
  YOTTA -> "YB"

kibiFileSizeAs :: BaseScale -> Types.FileOffset -> T.Text
kibiFileSizeAs bScale = case bScale of
  BYTE -> asB
  KILO -> asKi
  MEGA -> asMi
  GIGA -> asGi
  TERA -> asTi
  PETA -> asPi
  EXA -> asEi
  ZETTA -> asZi
  YOTTA -> asYi

siFileSizeAs :: BaseScale -> Types.FileOffset -> T.Text
siFileSizeAs bScale = case bScale of
  BYTE -> asB
  KILO -> asK
  MEGA -> asM
  GIGA -> asG
  TERA -> asT
  PETA -> asP
  EXA -> asE
  ZETTA -> asZ
  YOTTA -> asY

sizeK :: Num a => a
sizeK = 1000

sizeKi :: Num a => a
sizeKi = 1024

sizeM :: Num a => a
sizeM = sizeK * sizeK

sizeMi :: Num a => a
sizeMi = sizeKi * sizeKi

sizeG :: Num a => a
sizeG = sizeM * sizeK

sizeGi :: Num a => a
sizeGi = sizeMi * sizeKi

sizeT :: Num a => a
sizeT = sizeG * sizeK

sizeTi :: Num a => a
sizeTi = sizeGi * sizeKi

sizeP :: Num a => a
sizeP = sizeT * sizeK

sizePi :: Num a => a
sizePi = sizeTi * sizeKi

sizeE :: Num a => a
sizeE = sizeP * sizeK

sizeEi :: Num a => a
sizeEi = sizePi * sizeKi

sizeZ :: Num a => a
sizeZ = sizeE * sizeK

sizeZi :: Num a => a
sizeZi = sizeEi * sizeKi

sizeY :: Num a => a
sizeY = sizeZ * sizeK

sizeYi :: Num a => a
sizeYi = sizeZi * sizeKi

formatSize :: Double -> T.Text
formatSize r = T.pack size
  where
    size
      | r <= 9.9 = show $ ceiling' r 1
      | otherwise = show (ceiling r :: Int)

ceiling' :: Double -> Int -> Double
ceiling' r digitNum = fromIntegral upScaledCeil / scale
  where
    upScaledCeil = ceiling r' :: Int
    r' = r * scale
    scale = 10 ^ digitNum :: Double

asB :: Types.FileOffset -> T.Text
asB = T.pack . show

asK :: Types.FileOffset -> T.Text
asK offset = formatSize $ fromIntegral offset / sizeK

asKi :: Types.FileOffset -> T.Text
asKi offset = formatSize $ fromIntegral offset / sizeKi

asM :: Types.FileOffset -> T.Text
asM offset = formatSize $ fromIntegral offset / sizeM

asMi :: Types.FileOffset -> T.Text
asMi offset = formatSize $ fromIntegral offset / sizeMi

asG :: Types.FileOffset -> T.Text
asG offset = formatSize $ fromIntegral offset / sizeG

asGi :: Types.FileOffset -> T.Text
asGi offset = formatSize $ fromIntegral offset / sizeGi

asT :: Types.FileOffset -> T.Text
asT offset = formatSize $ fromIntegral offset / sizeT

asTi :: Types.FileOffset -> T.Text
asTi offset = formatSize $ fromIntegral offset / sizeTi

asP :: Types.FileOffset -> T.Text
asP offset = formatSize $ fromIntegral offset / sizeP

asPi :: Types.FileOffset -> T.Text
asPi offset = formatSize $ fromIntegral offset / sizePi

asE :: Types.FileOffset -> T.Text
asE offset = formatSize $ fromIntegral offset / sizeE

asEi :: Types.FileOffset -> T.Text
asEi offset = formatSize $ fromIntegral offset / sizeEi

asZ :: Types.FileOffset -> T.Text
asZ offset = formatSize $ fromIntegral offset / sizeZ

asZi :: Types.FileOffset -> T.Text
asZi offset = formatSize $ fromIntegral offset / sizeZi

asY :: Types.FileOffset -> T.Text
asY offset = formatSize $ fromIntegral offset / sizeY

asYi :: Types.FileOffset -> T.Text
asYi offset = formatSize $ fromIntegral offset / sizeYi
