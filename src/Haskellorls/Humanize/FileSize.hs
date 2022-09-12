-- |
-- = Humanize file size and block size
--
-- In this case, @Humanize@ means to make a number human readable format.
--
-- Two base scale sizes are used in the humanize context. We must select which
-- base scale size is used for humamize. Both of them are as below.
--
-- * 1024
-- * 1000
--
-- Scale sizes are power of base scale sizes. They are as below. @n@ is a
-- positive integer number.
--
-- * 1024 ^ n
-- * 1000 ^ n
--
-- If a file size is lesser than the base scale size, nothing is applied to the
-- file size. Otherwise apply a humanize conversion to the file size. In the
-- conversion, the file size is devided by the geatest number in scale sizes,
-- which are smallar than the file size. And then apply ceiling to the quosient
-- @q@. If the result is equal to or greater than @10.0@, treat it as an
-- integer number. Otherwise apply ceiling to @q@ until the second floating
-- poiint and treat the result as a real number. If these conversion is
-- applied, we should append a binary prefix or a metric prefix to indicate the
-- value is scaled. However, they don't define what prefix should be used in
-- this module. We should defined them in another module to format or view
-- humanized sizes.
module Haskellorls.Humanize.FileSize
  ( humanizeBI,
    humanizeSI,
    Scale (..),
    FileSize (..),
  )
where

-- | The scale base is 1024.
data BI

-- | The scale base is 1000.
data SI

-- | A modifier to indicate how much scaled the value is. This doesn't provide
-- base of scale, so somehow users should do it.
data Scale a b
  = NoScale b
  | Kilo b
  | Mega b
  | Giga b
  | Tera b
  | Peta b
  | Exa b
  | Zetta b
  | Yotta b
  deriving (Show, Functor)

-- | Human readable file size.
data FileSize
  = ISize Int
  | DSize Double
  deriving (Show)

-- | Human readable 'ceiling'. This applies 'ceiling' until the second decimal
-- point.
--
-- >>> ceiling' 0.0
-- 0.0
--
-- >>> ceiling' 1.001
-- 1.1
--
-- >>> ceiling' 10.911
-- 11.0
ceiling' :: Double -> Double
ceiling' d = fromIntegral @Int (ceiling @Double (d * 10)) / 10

-- | Convert a integral number into human readable format using a binary
-- prefix, which uses powers of 1024.
humanizeBI :: Integral a => a -> Scale BI FileSize
humanizeBI n = n `humanizeBy` 1024

-- | Convert a integral number into human readable format using a SI prefix,
-- which uses powers of 1000 instead of 1024.
humanizeSI :: Integral a => a -> Scale SI FileSize
humanizeSI n = n `humanizeBy` 1000

humanizeBy :: Integral a => a -> a -> Scale b FileSize
humanizeBy n scale
  | n <= scale ^ (0 :: Int) * (scale - 1) = NoScale . ISize $ fromIntegral n
  | n <= scale ^ (1 :: Int) * (scale - 1) = let r = fromIntegral n / fromIntegral scale ^ (1 :: Int) in Kilo $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
  | n <= scale ^ (2 :: Int) * (scale - 1) = let r = fromIntegral n / fromIntegral scale ^ (2 :: Int) in Mega $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
  | n <= scale ^ (3 :: Int) * (scale - 1) = let r = fromIntegral n / fromIntegral scale ^ (3 :: Int) in Giga $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
  | n <= scale ^ (4 :: Int) * (scale - 1) = let r = fromIntegral n / fromIntegral scale ^ (4 :: Int) in Tera $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
  | n <= scale ^ (5 :: Int) * (scale - 1) = let r = fromIntegral n / fromIntegral scale ^ (5 :: Int) in Peta $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
  | n <= scale ^ (6 :: Int) * (scale - 1) = let r = fromIntegral n / fromIntegral scale ^ (6 :: Int) in Exa $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
  | n <= scale ^ (7 :: Int) * (scale - 1) = let r = fromIntegral n / fromIntegral scale ^ (7 :: Int) in Zetta $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
  | otherwise = let r = fromIntegral n / fromIntegral scale ^ (8 :: Int) in Yotta $ if r > 9 then ISize $ ceiling r else DSize $ ceiling' r
