{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Haskellorls.Config.Option.SizeSpec (spec) where

import Data.List (inits)
import Haskellorls.Config.Option.Size
import Numeric
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = do
  describe "parseBlockSize" $ do
    it "returns Nothing if the value is empty." $ do
      parseBlockSize "" `shouldSatisfy` \case
        Nothing -> True
        _ -> False

    it "returns a block size with 'WithSeps' if the value is preceeded by @'@." do
      parseBlockSize "'1" `shouldSatisfy` \case
        Just (WithSeps (BlockSize 1)) -> True
        _ -> False

    it "returns KiloKibi if the value is just a \"K\"." $ do
      parseBlockSize "K" `shouldSatisfy` \case
        Just (NoMod KiloKibi) -> True
        _ -> False

    it "returns KiloKibi if the value is just a \"k\"." $ do
      parseBlockSize "k" `shouldSatisfy` \case
        Just (NoMod KiloKibi) -> True
        _ -> False

    it "returns MegaKibi if the value is just a \"M\"." $ do
      parseBlockSize "M" `shouldSatisfy` \case
        Just (NoMod MegaKibi) -> True
        _ -> False

    it "returns MegaKibi if the value is just a \"m\"." $ do
      parseBlockSize "m" `shouldSatisfy` \case
        Just (NoMod MegaKibi) -> True
        _ -> False

    it "returns GigaKibi if the value is just a \"G\"." $ do
      parseBlockSize "G" `shouldSatisfy` \case
        Just (NoMod GigaKibi) -> True
        _ -> False

    it "returns GigaKibi if the value is just a \"g\"." $ do
      parseBlockSize "g" `shouldSatisfy` \case
        Just (NoMod GigaKibi) -> True
        _ -> False

    it "returns TeraKibi if the value is just a \"T\"." $ do
      parseBlockSize "T" `shouldSatisfy` \case
        Just (NoMod TeraKibi) -> True
        _ -> False

    it "returns TeraKibi if the value is just a \"t\"." $ do
      parseBlockSize "t" `shouldSatisfy` \case
        Just (NoMod TeraKibi) -> True
        _ -> False

    it "returns PetaKibi if the value is just a \"P\"." $ do
      parseBlockSize "P" `shouldSatisfy` \case
        Just (NoMod PetaKibi) -> True
        _ -> False

    it "returns PetaKibi if the value is just a \"p\"." $ do
      parseBlockSize "p" `shouldSatisfy` \case
        Just (NoMod PetaKibi) -> True
        _ -> False

    it "returns ExaKibi if the value is just a \"E\"." $ do
      parseBlockSize "E" `shouldSatisfy` \case
        Just (NoMod ExaKibi) -> True
        _ -> False

    it "returns ExaKibi if the value is just a \"e\"." $ do
      parseBlockSize "e" `shouldSatisfy` \case
        Just (NoMod ExaKibi) -> True
        _ -> False

    it "returns ZettaKibi if the value is just a \"Z\"." $ do
      parseBlockSize "Z" `shouldSatisfy` \case
        Just (NoMod ZettaKibi) -> True
        _ -> False

    it "returns ZettaKibi if the value is just a \"z\"." $ do
      parseBlockSize "z" `shouldSatisfy` \case
        Just (NoMod ZettaKibi) -> True
        _ -> False

    it "returns YottaKibi if the value is just a \"Y\"." $ do
      parseBlockSize "Y" `shouldSatisfy` \case
        Just (NoMod YottaKibi) -> True
        _ -> False

    it "returns YottaKibi if the value is just a \"y\"." $ do
      parseBlockSize "y" `shouldSatisfy` \case
        Just (NoMod YottaKibi) -> True
        _ -> False

    it "returns KiloKibii if the value is just a \"KiB\"." $ do
      parseBlockSize "KiB" `shouldSatisfy` \case
        Just (NoMod KiloKibii) -> True
        _ -> False

    it "returns KiloKibii if the value is just a \"kiB\"." $ do
      parseBlockSize "kiB" `shouldSatisfy` \case
        Just (NoMod KiloKibii) -> True
        _ -> False

    it "returns MegaKibii if the value is just a \"MiB\"." $ do
      parseBlockSize "MiB" `shouldSatisfy` \case
        Just (NoMod MegaKibii) -> True
        _ -> False

    it "returns MegaKibii if the value is just a \"miB\"." $ do
      parseBlockSize "miB" `shouldSatisfy` \case
        Just (NoMod MegaKibii) -> True
        _ -> False

    it "returns GigaKibii if the value is just a \"GiB\"." $ do
      parseBlockSize "GiB" `shouldSatisfy` \case
        Just (NoMod GigaKibii) -> True
        _ -> False

    it "returns GigaKibii if the value is just a \"giB\"." $ do
      parseBlockSize "giB" `shouldSatisfy` \case
        Just (NoMod GigaKibii) -> True
        _ -> False

    it "returns TeraKibii if the value is just a \"TiB\"." $ do
      parseBlockSize "TiB" `shouldSatisfy` \case
        Just (NoMod TeraKibii) -> True
        _ -> False

    it "returns TeraKibii if the value is just a \"tiB\"." $ do
      parseBlockSize "tiB" `shouldSatisfy` \case
        Just (NoMod TeraKibii) -> True
        _ -> False

    it "returns PetaKibii if the value is just a \"PiB\"." $ do
      parseBlockSize "PiB" `shouldSatisfy` \case
        Just (NoMod PetaKibii) -> True
        _ -> False

    it "returns PetaKibii if the value is just a \"piB\"." $ do
      parseBlockSize "piB" `shouldSatisfy` \case
        Just (NoMod PetaKibii) -> True
        _ -> False

    it "returns ExaKibii if the value is just a \"EiB\"." $ do
      parseBlockSize "EiB" `shouldSatisfy` \case
        Just (NoMod ExaKibii) -> True
        _ -> False

    it "returns ExaKibii if the value is just a \"eiB\"." $ do
      parseBlockSize "eiB" `shouldSatisfy` \case
        Just (NoMod ExaKibii) -> True
        _ -> False

    it "returns ZettaKibii if the value is just a \"ZiB\"." $ do
      parseBlockSize "ZiB" `shouldSatisfy` \case
        Just (NoMod ZettaKibii) -> True
        _ -> False

    it "returns ZettaKibii if the value is just a \"ziB\"." $ do
      parseBlockSize "ziB" `shouldSatisfy` \case
        Just (NoMod ZettaKibii) -> True
        _ -> False

    it "returns YottaKibii if the value is just a \"YiB\"." $ do
      parseBlockSize "YiB" `shouldSatisfy` \case
        Just (NoMod YottaKibii) -> True
        _ -> False

    it "returns YottaKibii if the value is just a \"yiB\"." $ do
      parseBlockSize "yiB" `shouldSatisfy` \case
        Just (NoMod YottaKibii) -> True
        _ -> False

    it "returns KiloSi if the value is just a \"KB\"." $ do
      parseBlockSize "KB" `shouldSatisfy` \case
        Just (NoMod KiloSi) -> True
        _ -> False

    it "returns KiloSi if the value is just a \"kB\"." $ do
      parseBlockSize "kB" `shouldSatisfy` \case
        Just (NoMod KiloSi) -> True
        _ -> False

    it "returns MegaSi if the value is just a \"MB\"." $ do
      parseBlockSize "MB" `shouldSatisfy` \case
        Just (NoMod MegaSi) -> True
        _ -> False

    it "returns MegaSi if the value is just a \"mB\"." $ do
      parseBlockSize "mB" `shouldSatisfy` \case
        Just (NoMod MegaSi) -> True
        _ -> False

    it "returns GigaSi if the value is just a \"GB\"." $ do
      parseBlockSize "GB" `shouldSatisfy` \case
        Just (NoMod GigaSi) -> True
        _ -> False

    it "returns GigaSi if the value is just a \"gB\"." $ do
      parseBlockSize "gB" `shouldSatisfy` \case
        Just (NoMod GigaSi) -> True
        _ -> False

    it "returns TeraSi if the value is just a \"TB\"." $ do
      parseBlockSize "TB" `shouldSatisfy` \case
        Just (NoMod TeraSi) -> True
        _ -> False

    it "returns TeraSi if the value is just a \"tB\"." $ do
      parseBlockSize "tB" `shouldSatisfy` \case
        Just (NoMod TeraSi) -> True
        _ -> False

    it "returns PetaSi if the value is just a \"PB\"." $ do
      parseBlockSize "PB" `shouldSatisfy` \case
        Just (NoMod PetaSi) -> True
        _ -> False

    it "returns PetaSi if the value is just a \"pB\"." $ do
      parseBlockSize "pB" `shouldSatisfy` \case
        Just (NoMod PetaSi) -> True
        _ -> False

    it "returns ExaSi if the value is just a \"EB\"." $ do
      parseBlockSize "EB" `shouldSatisfy` \case
        Just (NoMod ExaSi) -> True
        _ -> False

    it "returns ExaSi if the value is just a \"eB\"." $ do
      parseBlockSize "eB" `shouldSatisfy` \case
        Just (NoMod ExaSi) -> True
        _ -> False

    it "returns ZettaSi if the value is just a \"ZB\"." $ do
      parseBlockSize "ZB" `shouldSatisfy` \case
        Just (NoMod ZettaSi) -> True
        _ -> False

    it "returns ZettaSi if the value is just a \"zB\"." $ do
      parseBlockSize "zB" `shouldSatisfy` \case
        Just (NoMod ZettaSi) -> True
        _ -> False

    it "returns YottaSi if the value is just a \"YB\"." $ do
      parseBlockSize "YB" `shouldSatisfy` \case
        Just (NoMod YottaSi) -> True
        _ -> False

    it "returns YottaSi if the value is just a \"yB\"." $ do
      parseBlockSize "yB" `shouldSatisfy` \case
        Just (NoMod YottaSi) -> True
        _ -> False

    it "returns HumanReadableBI if the value is the prefix of \"human\"." $ do
      property . forAll (elements . tail $ inits "human-readable") $ \s ->
        parseBlockSize s `shouldSatisfy` \case
          Just (NoMod HumanReadableBI) -> True
          _ -> False

    it "doesn't return HumanReadableBI if the value isn't just the prefix of \"human\"." $ do
      parseBlockSize "fooh" `shouldSatisfy` \case
        Just (NoMod HumanReadableBI) -> False
        _ -> True

      parseBlockSize "foohuman" `shouldSatisfy` \case
        Just (NoMod HumanReadableBI) -> False
        _ -> True

      parseBlockSize "hfoo" `shouldSatisfy` \case
        Just (NoMod HumanReadableBI) -> False
        _ -> True

      parseBlockSize "humanfoo" `shouldSatisfy` \case
        Just (NoMod HumanReadableBI) -> False
        _ -> True

    it "returns HumanReadableSI if the value is the prefix of \"si\"." $ do
      property . forAll (elements . tail $ inits "si") $ \s ->
        parseBlockSize s `shouldSatisfy` \case
          Just (NoMod HumanReadableSI) -> True
          _ -> False

    it "doesn't return HumanReadableSI if the value isn't just the prefix of \"si\"." $ do
      parseBlockSize "foos" `shouldSatisfy` \case
        Just (NoMod HumanReadableSI) -> False
        _ -> True

      parseBlockSize "foosi" `shouldSatisfy` \case
        Just (NoMod HumanReadableSI) -> False
        _ -> True

      parseBlockSize "sfoo" `shouldSatisfy` \case
        Just (NoMod HumanReadableSI) -> False
        _ -> True

      parseBlockSize "sifoo" `shouldSatisfy` \case
        Just (NoMod HumanReadableSI) -> False
        _ -> True

    it "returns Nothing if the value is just a character but not \"K\", \"k\" \"M\", \"m\", \"G\", \"g\", \"T\", \"t\", \"P\", \"p\", \"E\", \"e\", \"Z\", \"z\", \"Y\", \"y\", \"h\" or a number." $ do
      property . forAll (suchThat chooseAny $ \c -> c `notElem` "KkMmGgTtPpEeZzYyh0123456789") $ \c ->
        parseBlockSize [c] `shouldSatisfy` \case
          Nothing -> True
          _ -> False

    it "returns BlockSize n if the value is just a positive decimal number." $ do
      property . forAll (suchThat chooseAny $ \i -> i > 0) $ \i ->
        parseBlockSize (show i) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> i == n
          _ -> False

    it "returns BlockSize n if the value is just a positive octal number." $ do
      property . forAll (suchThat chooseAny $ \i -> i > 0) $ \i ->
        parseBlockSize ("0" <> showOct i "") `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> i == n
          _ -> False

    it "returns BlockSize n if the value is just a positive octal number even if some \"0\" is prefixed." $ do
      property . forAll (suchThat chooseAny $ \(i, j) -> i > 0 && j > 0) $ \(i, j) ->
        parseBlockSize ('0' : replicate (i `mod` 10) '0' <> showOct @Int j "") `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> j == n
          _ -> False

    it "returns BlockSize n if the value is just a positive hexadecimal number." $ do
      property . forAll (suchThat chooseAny $ \i -> i > 0) $ \i ->
        parseBlockSize ("0x" <> showHex i "") `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> i == n
          _ -> False

    it "returns BlockSize 0 if the value is just a positive hexadecimal number with some \"0\" as a prefix." $ do
      property . forAll (suchThat chooseAny $ \(i, j) -> i > 0 && j > 0) $ \(i, j) ->
        parseBlockSize (replicate (i `mod` 10) '0' <> "00x" <> showHex @Int j "") `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == 0
          _ -> False

    it "returns Nothing if the value is just a \"0\"." $ do
      parseBlockSize "0" `shouldSatisfy` \case
        Nothing -> True
        _ -> False

    it "returns Nothing if the value is just a negative number." $ do
      property . forAll (suchThat chooseAny $ \i -> i < (0 :: Int)) $ \i ->
        parseBlockSize (show i) `shouldSatisfy` \case
          Nothing -> True
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"K\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "K" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"k\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "k" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"M\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "M" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"m\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "m" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"G\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "G" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"g\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "g" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"T\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "T" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"t\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "t" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"P\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "P" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"p\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "p" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"E\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "E" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"e\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "e" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"Z\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "Z" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"z\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "z" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"Y\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "Y" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"y\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        parseBlockSize (show @Int i <> "y" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"KiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "K" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"kiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "k" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"MiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "MiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"miB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "miB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"GiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "GiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"giB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "giB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"TiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "TiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"tiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "tiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"PiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "PiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"piB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "piB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"EiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "EiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"eiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "eiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"ZiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "ZiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"ziB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "ziB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"YiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "YiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"yiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "yiB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 if the suffix of the value is \"KB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "KB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000
          _ -> False

    it "returns BlockSize n * 1000 if the suffix of the value is \"kB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "kB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000
          _ -> False

    it "returns BlockSize n * 1000 ^ 2 if the suffix of the value is \"MB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "MB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 2 if the suffix of the value is \"mB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "mB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 3 if the suffix of the value is \"GB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "GB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 3 if the suffix of the value is \"gB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "gB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 4 if the suffix of the value is \"TB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "TB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 4 if the suffix of the value is \"tB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "tB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 5 if the suffix of the value is \"PB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "PB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 5 if the suffix of the value is \"tB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "pB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 6 if the suffix of the value is \"EB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "EB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 6 if the suffix of the value is \"eB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "eB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 7 if the suffix of the value is \"ZB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "ZB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 7 if the suffix of the value is \"zB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "zB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 8 if the suffix of the value is \"YB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "YB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 8 if the suffix of the value is \"yB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        parseBlockSize (show @Int i <> "yB" <> s) `shouldSatisfy` \case
          Just (NoMod (BlockSize n)) -> n == i * 1000 ^ (8 :: Int)
          _ -> False
