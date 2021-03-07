{-# LANGUAGE LambdaCase #-}

module Haskellorls.Size.Utils
  ( lookupBlockSize,
  )
where

import Data.Functor
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Haskellorls.Option as Option
import Haskellorls.Size.Option
import qualified System.Environment as Env

lookupBlockSizeEnvVar :: IO (Maybe String)
lookupBlockSizeEnvVar =
  Env.lookupEnv "LS_BLOCK_SIZE" >>= \case
    Nothing -> Env.lookupEnv "BLOCK_SIZE"
    size -> pure size

lookupBlockSize :: Option.Option -> IO BlockSize
lookupBlockSize opt = case Option.blockSize opt of
  DefaultSize
    | Option.kibibyte opt -> pure DefaultSize
    | otherwise -> do
      size <- lookupBlockSizeEnvVar <&> maybe Nothing (parseBlockSize . T.pack)
      return $ Maybe.fromMaybe DefaultSize size
  size -> pure size
