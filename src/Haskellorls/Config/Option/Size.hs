module Haskellorls.Config.Option.Size
  ( blockSizeParser,
    parseBlockSize,
    siParser,
    humanReadableParser,
    module Haskellorls.Config.Size,
  )
where

import Data.Char
import Data.List (isPrefixOf)
import Haskellorls.Config.Size
import Numeric
import Options.Applicative

humanReadableParser :: Parser Bool
humanReadableParser =
  switch $
    short 'h'
      <> long "human-readable"
      <> help "Enable human readable output about file size (e.g. 1K, 23M)"

siParser :: Parser Bool
siParser =
  switch $
    long "si"
      <> help "Use 1000 as file size power instead of 1024"

blockSizeParser :: Parser BlockSize
blockSizeParser =
  option reader $
    long "block-size"
      <> metavar "SIZE"
      <> value DefaultSize
      <> help "Specify size unit when output file size"
  where
    reader = str >>= blockSizeReader

blockSizeReader :: String -> ReadM BlockSize
blockSizeReader s = case parseBlockSize s of
  Just size -> return size
  Nothing -> readerError "Invalid unit"

-- FIXME: This doesn't handle any overflow.
parseBlockSize :: String -> Maybe BlockSize
parseBlockSize s = case readNum s of
  [(n, "")]
    | n == 0 -> Nothing
    | otherwise -> Just . BlockSize $ n
  [(n, c : 'B' : _)] -> case toUpper c of
    'K' -> Just . BlockSize $ n * 1000 ^ (1 :: Int)
    'M' -> Just . BlockSize $ n * 1000 ^ (2 :: Int)
    'G' -> Just . BlockSize $ n * 1000 ^ (3 :: Int)
    'T' -> Just . BlockSize $ n * 1000 ^ (4 :: Int)
    'P' -> Just . BlockSize $ n * 1000 ^ (5 :: Int)
    'E' -> Just . BlockSize $ n * 1000 ^ (6 :: Int)
    'Z' -> Just . BlockSize $ n * 1000 ^ (7 :: Int)
    'Y' -> Just . BlockSize $ n * 1000 ^ (8 :: Int)
    _ -> Just . BlockSize $ n
  [(n, c : _)] -> case toUpper c of
    'K' -> Just . BlockSize $ n * 1024 ^ (1 :: Int)
    'M' -> Just . BlockSize $ n * 1024 ^ (2 :: Int)
    'G' -> Just . BlockSize $ n * 1024 ^ (3 :: Int)
    'T' -> Just . BlockSize $ n * 1024 ^ (4 :: Int)
    'P' -> Just . BlockSize $ n * 1024 ^ (5 :: Int)
    'E' -> Just . BlockSize $ n * 1024 ^ (6 :: Int)
    'Z' -> Just . BlockSize $ n * 1024 ^ (7 :: Int)
    'Y' -> Just . BlockSize $ n * 1024 ^ (8 :: Int)
    _ -> Just . BlockSize $ n
  _
    | null s -> Nothing
    | s `isPrefixOf` "human" -> Just HumanReadable
    | otherwise -> case s of
        c : "B" -> case toUpper c of
          'K' -> Just KiloSi
          'M' -> Just MegaSi
          'G' -> Just GigaSi
          'T' -> Just TeraSi
          'P' -> Just PetaSi
          'E' -> Just ExaSi
          'Z' -> Just ZettaSi
          'Y' -> Just YottaSi
          _ -> Nothing
        c : "iB" -> case toUpper c of
          'K' -> Just KiloKibii
          'M' -> Just MegaKibii
          'G' -> Just GigaKibii
          'T' -> Just TeraKibii
          'P' -> Just PetaKibii
          'E' -> Just ExaKibii
          'Z' -> Just ZettaKibii
          'Y' -> Just YottaKibii
          _ -> Nothing
        [c] -> case toUpper c of
          'K' -> Just KiloKibi
          'M' -> Just MegaKibi
          'G' -> Just GigaKibi
          'T' -> Just TeraKibi
          'P' -> Just PetaKibi
          'E' -> Just ExaKibi
          'Z' -> Just ZettaKibi
          'Y' -> Just YottaKibi
          _ -> Nothing
        _ -> Nothing

-- | Composed version of 'readDec', 'readOct' and 'readHex'.
--
-- >>> readNum "0"
-- [(0, "")]
--
-- >>> readNum "007"
-- [(7, "")]
--
-- >>> readNum "0xff"
-- [(255, "")]
--
-- >>> readNum "00xff"
-- [(0, "xff")]
readNum :: (Eq a, Num a) => ReadS a
readNum ('0' : '0' : s) = readOct $ '0' : s
readNum ('0' : c : s) = case toUpper c of
  'X' -> readHex s
  _ -> readOct $ c : s
readNum s = readDec s
