{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Haskellorls.Size.Option
  ( blockSizeParser,
    parseBlockSize,
    siParser,
    humanReadableParser,
    module Haskellorls.Size.Type,
  )
where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import Haskellorls.Size.Type
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

blockSizeReader :: T.Text -> ReadM BlockSize
blockSizeReader s = case parseBlockSize s of
  Just size -> return size
  Nothing -> readerError "Invalid unit"

parseBlockSize :: T.Text -> Maybe BlockSize
parseBlockSize s | s `L.elem` T.inits "human" = Just HumanReadable
parseBlockSize s = case parseLabel $ normalizeUnit unit of
  Just (baseScale, scaleSuffix) -> Just $ BlockSize {..}
  _ -> Nothing
  where
    scale = toScale sVal
    (sVal, unit) = T.span C.isDigit s

normalizeUnit :: T.Text -> T.Text
normalizeUnit = maybe "" (\(c, cs) -> C.toUpper c `T.cons` cs) . T.uncons

parseLabel :: T.Text -> Maybe (BaseScale, ScaleSuffix)
parseLabel t
  | T.length t > 3 = Nothing
  | T.null t = Just (BYTE, NONE)
  | otherwise = bScale >>= \bs -> sSuffix >>= \ss -> Just (bs, ss)
  where
    bScale = toBaseScale . T.singleton $ T.head t
    sSuffix = toScaleSuffix $ T.tail t

toScale :: T.Text -> Scale
toScale t
  | T.null t = NoScale
  | otherwise = Scale . read $ T.unpack t

toBaseScale :: T.Text -> Maybe BaseScale
toBaseScale s = case s of
  "" -> Just BYTE
  "K" -> Just KILO
  "M" -> Just MEGA
  "G" -> Just GIGA
  "T" -> Just TERA
  "P" -> Just PETA
  "E" -> Just EXA
  "Z" -> Just ZETTA
  "Y" -> Just YOTTA
  _ -> Nothing

toScaleSuffix :: T.Text -> Maybe ScaleSuffix
toScaleSuffix s = case s of
  "iB" -> Just KIBII
  "B" -> Just SI
  "" -> Just KIBI
  _ -> Nothing
