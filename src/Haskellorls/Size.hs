-- NOTE: When `--block-size` option is passed, file size format are `4K`,
-- `1111K` not `4.0K`, `1111.0K`

module Haskellorls.Size
  ( blockSizeTypeFrom,
    fileSizeFuncFor,
    coloredFileSizeFuncFor,
  )
where

import qualified Haskellorls.Color as Color
import qualified Haskellorls.NodeInfo as NodeInfo
import qualified Haskellorls.YetAnotherString as YAString
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

blockSizeTypeFrom :: String -> BlockSizeType
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

fileSizeFuncFor :: BlockSizeType -> (NodeInfo.NodeInfo -> String)
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

coloredFileSizeFuncFor :: BlockSizeType -> (Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString])
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

fileSizeAsHuman :: NodeInfo.NodeInfo -> String
fileSizeAsHuman node = f node
  where
    f = fileSizeFuncFor . detectBlockSizeType $ fileSizeOf node

coloredFileSizeAsHuman :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsHuman config node = f config node
  where
    f = coloredFileSizeFuncFor . detectBlockSizeType $ fileSizeOf node

fileSizeAsHumani :: NodeInfo.NodeInfo -> String
fileSizeAsHumani node = f node
  where
    f = fileSizeFuncFor . detectKibiBlockSizeType $ fileSizeOf node

coloredFileSizeAsHumani :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsHumani config node = f config node
  where
    f = coloredFileSizeFuncFor . detectKibiBlockSizeType $ fileSizeOf node

fileSizeAsByte :: NodeInfo.NodeInfo -> String
fileSizeAsByte = asB . fileSizeOf

coloredFileSizeAsByte :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsByte config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = asB $ fileSizeOf node,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    numberEscSeq = Color.fileSizeNumberEscapeSequence $ Color.extensionColorConfig config

fileSizeAsKilo :: NodeInfo.NodeInfo -> String
fileSizeAsKilo node = size ++ unitK
  where
    size = asK $ fileSizeOf node

coloredFileSizeAsKilo :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsKilo config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitK,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asK $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberKiloEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitKiloEscapeSequence $ Color.extensionColorConfig config

fileSizeAsKiloi :: NodeInfo.NodeInfo -> String
fileSizeAsKiloi node = size ++ unitKi
  where
    size = asKi $ fileSizeOf node

coloredFileSizeAsKiloi :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsKiloi config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitKi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asKi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberKiloEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitKiloEscapeSequence $ Color.extensionColorConfig config

fileSizeAsMega :: NodeInfo.NodeInfo -> String
fileSizeAsMega node = size ++ unitM
  where
    size = asM $ fileSizeOf node

coloredFileSizeAsMega :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsMega config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitM,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asM $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberMegaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitMegaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsMegai :: NodeInfo.NodeInfo -> String
fileSizeAsMegai node = size ++ unitMi
  where
    size = asMi $ fileSizeOf node

coloredFileSizeAsMegai :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsMegai config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitMi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asMi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberMegaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitMegaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsGiga :: NodeInfo.NodeInfo -> String
fileSizeAsGiga node = size ++ unitG
  where
    size = asG $ fileSizeOf node

coloredFileSizeAsGiga :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsGiga config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitG,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asG $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberGigaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitGigaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsGigai :: NodeInfo.NodeInfo -> String
fileSizeAsGigai node = size ++ unitGi
  where
    size = asGi $ fileSizeOf node

coloredFileSizeAsGigai :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsGigai config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitGi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asGi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberGigaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitGigaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsTera :: NodeInfo.NodeInfo -> String
fileSizeAsTera node = size ++ unitT
  where
    size = asT $ fileSizeOf node

coloredFileSizeAsTera :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsTera config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitT,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asT $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberTeraEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitTeraEscapeSequence $ Color.extensionColorConfig config

fileSizeAsTerai :: NodeInfo.NodeInfo -> String
fileSizeAsTerai node = size ++ unitTi
  where
    size = asTi $ fileSizeOf node

coloredFileSizeAsTerai :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsTerai config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitTi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asTi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberTeraEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitTeraEscapeSequence $ Color.extensionColorConfig config

fileSizeAsPeta :: NodeInfo.NodeInfo -> String
fileSizeAsPeta node = size ++ unitP
  where
    size = asP $ fileSizeOf node

coloredFileSizeAsPeta :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsPeta config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitP,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asP $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberPetaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitPetaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsPetai :: NodeInfo.NodeInfo -> String
fileSizeAsPetai node = size ++ unitPi
  where
    size = asPi $ fileSizeOf node

coloredFileSizeAsPetai :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsPetai config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitPi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asPi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberPetaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitPetaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsExa :: NodeInfo.NodeInfo -> String
fileSizeAsExa node = size ++ unitE
  where
    size = asE $ fileSizeOf node

coloredFileSizeAsExa :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsExa config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitE,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asE $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberExaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitExaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsExai :: NodeInfo.NodeInfo -> String
fileSizeAsExai node = size ++ unitEi
  where
    size = asEi $ fileSizeOf node

coloredFileSizeAsExai :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsExai config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitEi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asEi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberExaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitExaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsZetta :: NodeInfo.NodeInfo -> String
fileSizeAsZetta node = size ++ unitZ
  where
    size = asZ $ fileSizeOf node

coloredFileSizeAsZetta :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsZetta config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitZ,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asZ $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberZettaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitZettaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsZettai :: NodeInfo.NodeInfo -> String
fileSizeAsZettai node = size ++ unitZi
  where
    size = asZi $ fileSizeOf node

coloredFileSizeAsZettai :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsZettai config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitZi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asZi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberZettaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitZettaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsYotta :: NodeInfo.NodeInfo -> String
fileSizeAsYotta node = size ++ unitY
  where
    size = asY $ fileSizeOf node

coloredFileSizeAsYotta :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsYotta config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitY,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asY $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberYottaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitYottaEscapeSequence $ Color.extensionColorConfig config

fileSizeAsYottai :: NodeInfo.NodeInfo -> String
fileSizeAsYottai node = size ++ unitYi
  where
    size = asYi $ fileSizeOf node

coloredFileSizeAsYottai :: Color.Config -> NodeInfo.NodeInfo -> [YAString.WrapedString]
coloredFileSizeAsYottai config node =
  [ YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config numberEscSeq,
        YAString.wrappedStringMain = size,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      },
    YAString.WrapedString
      { YAString.wrappedStringPrefix = Color.applyEscapeSequence config unitEscSeq,
        YAString.wrappedStringMain = unitYi,
        YAString.wrappedStringSuffix = Color.applyEscapeSequence config ""
      }
  ]
  where
    size = asYi $ fileSizeOf node
    numberEscSeq = Color.fileSizeNumberYottaEscapeSequence $ Color.extensionColorConfig config
    unitEscSeq = Color.fileSizeUnitYottaEscapeSequence $ Color.extensionColorConfig config

-- Const {{{
-- Unit {{{
unitK :: String
unitK = "KB"

unitKi :: String
unitKi = "K"

unitM :: String
unitM = "MB"

unitMi :: String
unitMi = "M"

unitG :: String
unitG = "GB"

unitGi :: String
unitGi = "G"

unitT :: String
unitT = "TB"

unitTi :: String
unitTi = "T"

unitP :: String
unitP = "PB"

unitPi :: String
unitPi = "P"

unitE :: String
unitE = "EB"

unitEi :: String
unitEi = "E"

unitZ :: String
unitZ = "ZB"

unitZi :: String
unitZi = "Z"

unitY :: String
unitY = "YB"

unitYi :: String
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
formatSize :: Double -> String
formatSize r
  | r <= 9.9 = show $ ceiling' r 1
  | otherwise = show (ceiling r :: Int)

ceiling' :: Double -> Int -> Double
ceiling' r digitNum = fromIntegral upScaledCeil / scale
  where
    upScaledCeil = ceiling r' :: Int
    r' = r * scale
    scale = 10 ^ digitNum :: Double

asB :: Types.FileOffset -> String
asB = show

asK :: Types.FileOffset -> String
asK offset = formatSize $ fromIntegral offset / sizeK

asKi :: Types.FileOffset -> String
asKi offset = formatSize $ fromIntegral offset / sizeKi

asM :: Types.FileOffset -> String
asM offset = formatSize $ fromIntegral offset / sizeM

asMi :: Types.FileOffset -> String
asMi offset = formatSize $ fromIntegral offset / sizeMi

asG :: Types.FileOffset -> String
asG offset = formatSize $ fromIntegral offset / sizeG

asGi :: Types.FileOffset -> String
asGi offset = formatSize $ fromIntegral offset / sizeGi

asT :: Types.FileOffset -> String
asT offset = formatSize $ fromIntegral offset / sizeT

asTi :: Types.FileOffset -> String
asTi offset = formatSize $ fromIntegral offset / sizeTi

asP :: Types.FileOffset -> String
asP offset = formatSize $ fromIntegral offset / sizeP

asPi :: Types.FileOffset -> String
asPi offset = formatSize $ fromIntegral offset / sizePi

asE :: Types.FileOffset -> String
asE offset = formatSize $ fromIntegral offset / sizeE

asEi :: Types.FileOffset -> String
asEi offset = formatSize $ fromIntegral offset / sizeEi

asZ :: Types.FileOffset -> String
asZ offset = formatSize $ fromIntegral offset / sizeZ

asZi :: Types.FileOffset -> String
asZi offset = formatSize $ fromIntegral offset / sizeZi

asY :: Types.FileOffset -> String
asY offset = formatSize $ fromIntegral offset / sizeY

asYi :: Types.FileOffset -> String
asYi offset = formatSize $ fromIntegral offset / sizeYi

-- }}}
