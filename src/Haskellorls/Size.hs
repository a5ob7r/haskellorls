{-# LANGUAGE OverloadedStrings #-}

-- NOTE: When `--block-size` option is passed, file size format are `4K`,
-- `1111K` not `4.0K`, `1111.0K`

module Haskellorls.Size
  ( blockSizeTypeFrom,
    fileSizeFuncFor,
    coloredFileSizeFuncFor,
  )
where

import qualified Data.Text as T
import qualified Haskellorls.Color as Color
import qualified Haskellorls.NodeInfo as NodeInfo
import qualified Haskellorls.WrappedText as WT
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Types

data BlockSizeType
  = BYTE
  | KILO
  | KILOi
  | MEGA
  | MEGAi
  | GIGA
  | GIGAi
  | TERA
  | TERAi
  | PETA
  | PETAi
  | EXA
  | EXAi
  | ZETTA
  | ZETTAi
  | YOTTA
  | YOTTAi
  | HUMAN
  | HUMANi

blockSizeTypeFrom :: T.Text -> BlockSizeType
blockSizeTypeFrom s = case s of
  "KB" -> KILO
  "K" -> KILOi
  "KiB" -> KILOi
  "MB" -> MEGA
  "M" -> MEGAi
  "MiB" -> MEGAi
  "GB" -> GIGA
  "GiB" -> GIGAi
  "G" -> GIGAi
  "TB" -> TERA
  "TiB" -> TERAi
  "T" -> TERAi
  "PB" -> PETA
  "PiB" -> PETAi
  "P" -> PETAi
  "EB" -> EXA
  "EiB" -> EXAi
  "E" -> EXAi
  "ZB" -> ZETTA
  "ZiB" -> ZETTAi
  "Z" -> ZETTAi
  "YB" -> YOTTA
  "YiB" -> YOTTAi
  "Y" -> YOTTAi
  "HUMAN" -> HUMAN
  "HUMANi" -> HUMANi
  _ -> BYTE

fileSizeFuncFor :: BlockSizeType -> (NodeInfo.NodeInfo -> T.Text)
fileSizeFuncFor blockSizeType = case blockSizeType of
  BYTE -> fileSizeAsByte
  KILO -> fileSizeAsKilo
  KILOi -> fileSizeAsKiloi
  MEGA -> fileSizeAsMega
  MEGAi -> fileSizeAsMegai
  GIGA -> fileSizeAsGiga
  GIGAi -> fileSizeAsGigai
  TERA -> fileSizeAsTera
  TERAi -> fileSizeAsTerai
  PETA -> fileSizeAsPeta
  PETAi -> fileSizeAsPetai
  EXA -> fileSizeAsExa
  EXAi -> fileSizeAsExai
  ZETTA -> fileSizeAsZetta
  ZETTAi -> fileSizeAsZettai
  YOTTA -> fileSizeAsYotta
  YOTTAi -> fileSizeAsYottai
  HUMAN -> fileSizeAsHuman
  HUMANi -> fileSizeAsHumani

coloredFileSizeFuncFor :: BlockSizeType -> (Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText])
coloredFileSizeFuncFor blockSizeType = case blockSizeType of
  BYTE -> coloredFileSizeAsByte
  KILO -> coloredFileSizeAsKilo
  KILOi -> coloredFileSizeAsKiloi
  MEGA -> coloredFileSizeAsMega
  MEGAi -> coloredFileSizeAsMegai
  GIGA -> coloredFileSizeAsGiga
  GIGAi -> coloredFileSizeAsGigai
  TERA -> coloredFileSizeAsTera
  TERAi -> coloredFileSizeAsTerai
  PETA -> coloredFileSizeAsPeta
  PETAi -> coloredFileSizeAsPetai
  EXA -> coloredFileSizeAsExa
  EXAi -> coloredFileSizeAsExai
  ZETTA -> coloredFileSizeAsZetta
  ZETTAi -> coloredFileSizeAsZettai
  YOTTA -> coloredFileSizeAsYotta
  YOTTAi -> coloredFileSizeAsYottai
  HUMAN -> coloredFileSizeAsHuman
  HUMANi -> coloredFileSizeAsHumani

detectBlockSizeType :: Types.FileOffset -> BlockSizeType
detectBlockSizeType offset
  | offset < sizeK = BYTE
  | offset < sizeM = KILO
  | offset < sizeG = MEGA
  | offset < sizeT = GIGA
  | offset < sizeP = TERA
  | offset < sizeE = PETA
  | offset < sizeZ = EXA
  | offset < sizeY = ZETTA
  | otherwise = YOTTA

detectKibiBlockSizeType :: Types.FileOffset -> BlockSizeType
detectKibiBlockSizeType offset
  | offset < sizeKi = BYTE
  | offset < sizeMi = KILOi
  | offset < sizeGi = MEGAi
  | offset < sizeTi = GIGAi
  | offset < sizePi = TERAi
  | offset < sizeEi = PETAi
  | offset < sizeZi = EXAi
  | offset < sizeYi = ZETTAi
  | otherwise = YOTTAi

fileSizeOf :: NodeInfo.NodeInfo -> Types.FileOffset
fileSizeOf = Files.fileSize . NodeInfo.nodeInfoStatus

fileSizeAsHuman :: NodeInfo.NodeInfo -> T.Text
fileSizeAsHuman node = f node
  where
    f = fileSizeFuncFor . detectBlockSizeType $ fileSizeOf node

coloredFileSizeAsHuman :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsHuman config node = f config node
  where
    f = coloredFileSizeFuncFor . detectBlockSizeType $ fileSizeOf node

fileSizeAsHumani :: NodeInfo.NodeInfo -> T.Text
fileSizeAsHumani node = f node
  where
    f = fileSizeFuncFor . detectKibiBlockSizeType $ fileSizeOf node

coloredFileSizeAsHumani :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsHumani config node = f config node
  where
    f = coloredFileSizeFuncFor . detectKibiBlockSizeType $ fileSizeOf node

fileSizeAsByte :: NodeInfo.NodeInfo -> T.Text
fileSizeAsByte = asB . fileSizeOf

coloredFileSizeAsByte :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsByte config node = [Color.toWrappedText config getter size]
  where
    size = asB $ fileSizeOf node
    getter = Color.fileSizeNumberEscapeSequence . Color.extensionColorConfig

fileSizeAsKilo :: NodeInfo.NodeInfo -> T.Text
fileSizeAsKilo node = size <> unitK
  where
    size = asK $ fileSizeOf node

coloredFileSizeAsKilo :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsKilo config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitK
  ]
  where
    size = asK $ fileSizeOf node
    getterNumber = Color.fileSizeNumberKiloEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitKiloEscapeSequence . Color.extensionColorConfig

fileSizeAsKiloi :: NodeInfo.NodeInfo -> T.Text
fileSizeAsKiloi node = size <> unitKi
  where
    size = asKi $ fileSizeOf node

coloredFileSizeAsKiloi :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsKiloi config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitKi
  ]
  where
    size = asKi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberKiloEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitKiloEscapeSequence . Color.extensionColorConfig

fileSizeAsMega :: NodeInfo.NodeInfo -> T.Text
fileSizeAsMega node = size <> unitM
  where
    size = asM $ fileSizeOf node

coloredFileSizeAsMega :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsMega config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitM
  ]
  where
    size = asM $ fileSizeOf node
    getterNumber = Color.fileSizeNumberMegaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitMegaEscapeSequence . Color.extensionColorConfig

fileSizeAsMegai :: NodeInfo.NodeInfo -> T.Text
fileSizeAsMegai node = size <> unitMi
  where
    size = asMi $ fileSizeOf node

coloredFileSizeAsMegai :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsMegai config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitMi
  ]
  where
    size = asMi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberMegaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitMegaEscapeSequence . Color.extensionColorConfig

fileSizeAsGiga :: NodeInfo.NodeInfo -> T.Text
fileSizeAsGiga node = size <> unitG
  where
    size = asG $ fileSizeOf node

coloredFileSizeAsGiga :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsGiga config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitG
  ]
  where
    size = asG $ fileSizeOf node
    getterNumber = Color.fileSizeNumberGigaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitGigaEscapeSequence . Color.extensionColorConfig

fileSizeAsGigai :: NodeInfo.NodeInfo -> T.Text
fileSizeAsGigai node = size <> unitGi
  where
    size = asGi $ fileSizeOf node

coloredFileSizeAsGigai :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsGigai config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitGi
  ]
  where
    size = asGi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberGigaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitGigaEscapeSequence . Color.extensionColorConfig

fileSizeAsTera :: NodeInfo.NodeInfo -> T.Text
fileSizeAsTera node = size <> unitT
  where
    size = asT $ fileSizeOf node

coloredFileSizeAsTera :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsTera config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitT
  ]
  where
    size = asT $ fileSizeOf node
    getterNumber = Color.fileSizeNumberTeraEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitTeraEscapeSequence . Color.extensionColorConfig

fileSizeAsTerai :: NodeInfo.NodeInfo -> T.Text
fileSizeAsTerai node = size <> unitTi
  where
    size = asTi $ fileSizeOf node

coloredFileSizeAsTerai :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsTerai config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitTi
  ]
  where
    size = asTi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberTeraEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitTeraEscapeSequence . Color.extensionColorConfig

fileSizeAsPeta :: NodeInfo.NodeInfo -> T.Text
fileSizeAsPeta node = size <> unitP
  where
    size = asP $ fileSizeOf node

coloredFileSizeAsPeta :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsPeta config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitP
  ]
  where
    size = asP $ fileSizeOf node
    getterNumber = Color.fileSizeNumberPetaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitPetaEscapeSequence . Color.extensionColorConfig

fileSizeAsPetai :: NodeInfo.NodeInfo -> T.Text
fileSizeAsPetai node = size <> unitPi
  where
    size = asPi $ fileSizeOf node

coloredFileSizeAsPetai :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsPetai config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitPi
  ]
  where
    size = asPi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberPetaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitPetaEscapeSequence . Color.extensionColorConfig

fileSizeAsExa :: NodeInfo.NodeInfo -> T.Text
fileSizeAsExa node = size <> unitE
  where
    size = asE $ fileSizeOf node

coloredFileSizeAsExa :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsExa config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitE
  ]
  where
    size = asE $ fileSizeOf node
    getterNumber = Color.fileSizeNumberExaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitExaEscapeSequence . Color.extensionColorConfig

fileSizeAsExai :: NodeInfo.NodeInfo -> T.Text
fileSizeAsExai node = size <> unitEi
  where
    size = asEi $ fileSizeOf node

coloredFileSizeAsExai :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsExai config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitEi
  ]
  where
    size = asEi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberExaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitExaEscapeSequence . Color.extensionColorConfig

fileSizeAsZetta :: NodeInfo.NodeInfo -> T.Text
fileSizeAsZetta node = size <> unitZ
  where
    size = asZ $ fileSizeOf node

coloredFileSizeAsZetta :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsZetta config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitZ
  ]
  where
    size = asZ $ fileSizeOf node
    getterNumber = Color.fileSizeNumberZettaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitZettaEscapeSequence . Color.extensionColorConfig

fileSizeAsZettai :: NodeInfo.NodeInfo -> T.Text
fileSizeAsZettai node = size <> unitZi
  where
    size = asZi $ fileSizeOf node

coloredFileSizeAsZettai :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsZettai config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitZi
  ]
  where
    size = asZi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberZettaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitZettaEscapeSequence . Color.extensionColorConfig

fileSizeAsYotta :: NodeInfo.NodeInfo -> T.Text
fileSizeAsYotta node = size <> unitY
  where
    size = asY $ fileSizeOf node

coloredFileSizeAsYotta :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsYotta config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitY
  ]
  where
    size = asY $ fileSizeOf node
    getterNumber = Color.fileSizeNumberYottaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitYottaEscapeSequence . Color.extensionColorConfig

fileSizeAsYottai :: NodeInfo.NodeInfo -> T.Text
fileSizeAsYottai node = size <> unitYi
  where
    size = asYi $ fileSizeOf node

coloredFileSizeAsYottai :: Color.Config -> NodeInfo.NodeInfo -> [WT.WrappedText]
coloredFileSizeAsYottai config node =
  [ Color.toWrappedText config getterNumber size,
    Color.toWrappedText config getterUnit unitYi
  ]
  where
    size = asYi $ fileSizeOf node
    getterNumber = Color.fileSizeNumberYottaEscapeSequence . Color.extensionColorConfig
    getterUnit = Color.fileSizeUnitYottaEscapeSequence . Color.extensionColorConfig

-- Const {{{
-- Unit {{{
unitK :: T.Text
unitK = "KB"

unitKi :: T.Text
unitKi = "K"

unitM :: T.Text
unitM = "MB"

unitMi :: T.Text
unitMi = "M"

unitG :: T.Text
unitG = "GB"

unitGi :: T.Text
unitGi = "G"

unitT :: T.Text
unitT = "TB"

unitTi :: T.Text
unitTi = "T"

unitP :: T.Text
unitP = "PB"

unitPi :: T.Text
unitPi = "P"

unitE :: T.Text
unitE = "EB"

unitEi :: T.Text
unitEi = "E"

unitZ :: T.Text
unitZ = "ZB"

unitZi :: T.Text
unitZi = "Z"

unitY :: T.Text
unitY = "YB"

unitYi :: T.Text
unitYi = "Y"

-- }}}

-- Size {{{
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

-- }}}
-- }}}

-- Format {{{
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

-- }}}
