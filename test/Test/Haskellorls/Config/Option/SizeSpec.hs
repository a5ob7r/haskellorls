{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Test.Haskellorls.Config.Option.SizeSpec (spec) where

import Data.List (inits)
import Haskellorls.Config.Size
import Numeric
import Test.QuickCheck
import Test.Syd
import Witch (tryFrom, tryInto)

spec :: Spec
spec = do
  describe "Witch.tryFrom" $ do
    it "returns Nothing if the value is empty." $ do
      tryInto @(BlockSizeMod BlockSize) "" `shouldSatisfy` \case
        Left _ -> True
        _ -> False

    it "returns a block size with 'WithSeps' if the value is preceeded by @'@." do
      tryFrom "'1" `shouldSatisfy` \case
        Right (WithSeps (BlockSize 1)) -> True
        _ -> False

    it "returns KiloKibi if the value is just a \"K\"." $ do
      tryFrom "K" `shouldSatisfy` \case
        Right (NoMod KiloKibi) -> True
        _ -> False

    it "returns KiloKibi if the value is just a \"k\"." $ do
      tryFrom "k" `shouldSatisfy` \case
        Right (NoMod KiloKibi) -> True
        _ -> False

    it "returns MegaKibi if the value is just a \"M\"." $ do
      tryFrom "M" `shouldSatisfy` \case
        Right (NoMod MegaKibi) -> True
        _ -> False

    it "returns MegaKibi if the value is just a \"m\"." $ do
      tryFrom "m" `shouldSatisfy` \case
        Right (NoMod MegaKibi) -> True
        _ -> False

    it "returns GigaKibi if the value is just a \"G\"." $ do
      tryFrom "G" `shouldSatisfy` \case
        Right (NoMod GigaKibi) -> True
        _ -> False

    it "returns GigaKibi if the value is just a \"g\"." $ do
      tryFrom "g" `shouldSatisfy` \case
        Right (NoMod GigaKibi) -> True
        _ -> False

    it "returns TeraKibi if the value is just a \"T\"." $ do
      tryFrom "T" `shouldSatisfy` \case
        Right (NoMod TeraKibi) -> True
        _ -> False

    it "returns TeraKibi if the value is just a \"t\"." $ do
      tryFrom "t" `shouldSatisfy` \case
        Right (NoMod TeraKibi) -> True
        _ -> False

    it "returns PetaKibi if the value is just a \"P\"." $ do
      tryFrom "P" `shouldSatisfy` \case
        Right (NoMod PetaKibi) -> True
        _ -> False

    it "returns PetaKibi if the value is just a \"p\"." $ do
      tryFrom "p" `shouldSatisfy` \case
        Right (NoMod PetaKibi) -> True
        _ -> False

    it "returns ExaKibi if the value is just a \"E\"." $ do
      tryFrom "E" `shouldSatisfy` \case
        Right (NoMod ExaKibi) -> True
        _ -> False

    it "returns ExaKibi if the value is just a \"e\"." $ do
      tryFrom "e" `shouldSatisfy` \case
        Right (NoMod ExaKibi) -> True
        _ -> False

    it "returns ZettaKibi if the value is just a \"Z\"." $ do
      tryFrom "Z" `shouldSatisfy` \case
        Right (NoMod ZettaKibi) -> True
        _ -> False

    it "returns ZettaKibi if the value is just a \"z\"." $ do
      tryFrom "z" `shouldSatisfy` \case
        Right (NoMod ZettaKibi) -> True
        _ -> False

    it "returns YottaKibi if the value is just a \"Y\"." $ do
      tryFrom "Y" `shouldSatisfy` \case
        Right (NoMod YottaKibi) -> True
        _ -> False

    it "returns YottaKibi if the value is just a \"y\"." $ do
      tryFrom "y" `shouldSatisfy` \case
        Right (NoMod YottaKibi) -> True
        _ -> False

    it "returns KiloKibii if the value is just a \"KiB\"." $ do
      tryFrom "KiB" `shouldSatisfy` \case
        Right (NoMod KiloKibii) -> True
        _ -> False

    it "returns KiloKibii if the value is just a \"kiB\"." $ do
      tryFrom "kiB" `shouldSatisfy` \case
        Right (NoMod KiloKibii) -> True
        _ -> False

    it "returns MegaKibii if the value is just a \"MiB\"." $ do
      tryFrom "MiB" `shouldSatisfy` \case
        Right (NoMod MegaKibii) -> True
        _ -> False

    it "returns MegaKibii if the value is just a \"miB\"." $ do
      tryFrom "miB" `shouldSatisfy` \case
        Right (NoMod MegaKibii) -> True
        _ -> False

    it "returns GigaKibii if the value is just a \"GiB\"." $ do
      tryFrom "GiB" `shouldSatisfy` \case
        Right (NoMod GigaKibii) -> True
        _ -> False

    it "returns GigaKibii if the value is just a \"giB\"." $ do
      tryFrom "giB" `shouldSatisfy` \case
        Right (NoMod GigaKibii) -> True
        _ -> False

    it "returns TeraKibii if the value is just a \"TiB\"." $ do
      tryFrom "TiB" `shouldSatisfy` \case
        Right (NoMod TeraKibii) -> True
        _ -> False

    it "returns TeraKibii if the value is just a \"tiB\"." $ do
      tryFrom "tiB" `shouldSatisfy` \case
        Right (NoMod TeraKibii) -> True
        _ -> False

    it "returns PetaKibii if the value is just a \"PiB\"." $ do
      tryFrom "PiB" `shouldSatisfy` \case
        Right (NoMod PetaKibii) -> True
        _ -> False

    it "returns PetaKibii if the value is just a \"piB\"." $ do
      tryFrom "piB" `shouldSatisfy` \case
        Right (NoMod PetaKibii) -> True
        _ -> False

    it "returns ExaKibii if the value is just a \"EiB\"." $ do
      tryFrom "EiB" `shouldSatisfy` \case
        Right (NoMod ExaKibii) -> True
        _ -> False

    it "returns ExaKibii if the value is just a \"eiB\"." $ do
      tryFrom "eiB" `shouldSatisfy` \case
        Right (NoMod ExaKibii) -> True
        _ -> False

    it "returns ZettaKibii if the value is just a \"ZiB\"." $ do
      tryFrom "ZiB" `shouldSatisfy` \case
        Right (NoMod ZettaKibii) -> True
        _ -> False

    it "returns ZettaKibii if the value is just a \"ziB\"." $ do
      tryFrom "ziB" `shouldSatisfy` \case
        Right (NoMod ZettaKibii) -> True
        _ -> False

    it "returns YottaKibii if the value is just a \"YiB\"." $ do
      tryFrom "YiB" `shouldSatisfy` \case
        Right (NoMod YottaKibii) -> True
        _ -> False

    it "returns YottaKibii if the value is just a \"yiB\"." $ do
      tryFrom "yiB" `shouldSatisfy` \case
        Right (NoMod YottaKibii) -> True
        _ -> False

    it "returns KiloSi if the value is just a \"KB\"." $ do
      tryFrom "KB" `shouldSatisfy` \case
        Right (NoMod KiloSi) -> True
        _ -> False

    it "returns KiloSi if the value is just a \"kB\"." $ do
      tryFrom "kB" `shouldSatisfy` \case
        Right (NoMod KiloSi) -> True
        _ -> False

    it "returns MegaSi if the value is just a \"MB\"." $ do
      tryFrom "MB" `shouldSatisfy` \case
        Right (NoMod MegaSi) -> True
        _ -> False

    it "returns MegaSi if the value is just a \"mB\"." $ do
      tryFrom "mB" `shouldSatisfy` \case
        Right (NoMod MegaSi) -> True
        _ -> False

    it "returns GigaSi if the value is just a \"GB\"." $ do
      tryFrom "GB" `shouldSatisfy` \case
        Right (NoMod GigaSi) -> True
        _ -> False

    it "returns GigaSi if the value is just a \"gB\"." $ do
      tryFrom "gB" `shouldSatisfy` \case
        Right (NoMod GigaSi) -> True
        _ -> False

    it "returns TeraSi if the value is just a \"TB\"." $ do
      tryFrom "TB" `shouldSatisfy` \case
        Right (NoMod TeraSi) -> True
        _ -> False

    it "returns TeraSi if the value is just a \"tB\"." $ do
      tryFrom "tB" `shouldSatisfy` \case
        Right (NoMod TeraSi) -> True
        _ -> False

    it "returns PetaSi if the value is just a \"PB\"." $ do
      tryFrom "PB" `shouldSatisfy` \case
        Right (NoMod PetaSi) -> True
        _ -> False

    it "returns PetaSi if the value is just a \"pB\"." $ do
      tryFrom "pB" `shouldSatisfy` \case
        Right (NoMod PetaSi) -> True
        _ -> False

    it "returns ExaSi if the value is just a \"EB\"." $ do
      tryFrom "EB" `shouldSatisfy` \case
        Right (NoMod ExaSi) -> True
        _ -> False

    it "returns ExaSi if the value is just a \"eB\"." $ do
      tryFrom "eB" `shouldSatisfy` \case
        Right (NoMod ExaSi) -> True
        _ -> False

    it "returns ZettaSi if the value is just a \"ZB\"." $ do
      tryFrom "ZB" `shouldSatisfy` \case
        Right (NoMod ZettaSi) -> True
        _ -> False

    it "returns ZettaSi if the value is just a \"zB\"." $ do
      tryFrom "zB" `shouldSatisfy` \case
        Right (NoMod ZettaSi) -> True
        _ -> False

    it "returns YottaSi if the value is just a \"YB\"." $ do
      tryFrom "YB" `shouldSatisfy` \case
        Right (NoMod YottaSi) -> True
        _ -> False

    it "returns YottaSi if the value is just a \"yB\"." $ do
      tryFrom "yB" `shouldSatisfy` \case
        Right (NoMod YottaSi) -> True
        _ -> False

    it "returns HumanReadableBI if the value is the prefix of \"human-readable\"." $ do
      property . forAll (elements . drop 1 $ inits "human-readable") $ \s ->
        tryFrom s `shouldSatisfy` \case
          Right (NoMod HumanReadableBI) -> True
          _ -> False

    it "doesn't return HumanReadableBI if the value isn't just the prefix of \"human-readable\"." $ do
      tryFrom "fooh" `shouldSatisfy` \case
        Right (NoMod HumanReadableBI) -> False
        _ -> True

      tryFrom "foohuman" `shouldSatisfy` \case
        Right (NoMod HumanReadableBI) -> False
        _ -> True

      tryFrom "hfoo" `shouldSatisfy` \case
        Right (NoMod HumanReadableBI) -> False
        _ -> True

      tryFrom "humanfoo" `shouldSatisfy` \case
        Right (NoMod HumanReadableBI) -> False
        _ -> True

      tryFrom "human-readablefoo" `shouldSatisfy` \case
        Right (NoMod HumanReadableBI) -> False
        _ -> True

    it "returns HumanReadableSI if the value is the prefix of \"si\"." $ do
      property . forAll (elements . drop 1 $ inits "si") $ \s ->
        tryFrom s `shouldSatisfy` \case
          Right (NoMod HumanReadableSI) -> True
          _ -> False

    it "doesn't return HumanReadableSI if the value isn't just the prefix of \"si\"." $ do
      tryFrom "foos" `shouldSatisfy` \case
        Right (NoMod HumanReadableSI) -> False
        _ -> True

      tryFrom "foosi" `shouldSatisfy` \case
        Right (NoMod HumanReadableSI) -> False
        _ -> True

      tryFrom "sfoo" `shouldSatisfy` \case
        Right (NoMod HumanReadableSI) -> False
        _ -> True

      tryFrom "sifoo" `shouldSatisfy` \case
        Right (NoMod HumanReadableSI) -> False
        _ -> True

    it "returns Nothing if the value is just a character but not \"K\", \"k\" \"M\", \"m\", \"G\", \"g\", \"T\", \"t\", \"P\", \"p\", \"E\", \"e\", \"Z\", \"z\", \"Y\", \"y\", \"h\" or a number." $ do
      property . forAll (suchThat chooseAny $ \c -> c `notElem` "KkMmGgTtPpEeZzYyh0123456789") $ \c ->
        tryInto @(BlockSizeMod BlockSize) [c] `shouldSatisfy` \case
          Left _ -> True
          _ -> False

    it "returns BlockSize n if the value is just a positive decimal number." $ do
      property . forAll (suchThat chooseAny $ \i -> i > 0) $ \i ->
        tryFrom (show i) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> i == n
          _ -> False

    it "returns BlockSize n if the value is just a positive octal number." $ do
      property . forAll (suchThat chooseAny $ \i -> i > 0) $ \i ->
        tryFrom ("0" <> showOct i "") `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> i == n
          _ -> False

    it "returns BlockSize n if the value is just a positive octal number even if some \"0\" is prefixed." $ do
      property . forAll (suchThat chooseAny $ \(i, j) -> i > 0 && j > 0) $ \(i, j) ->
        tryFrom ('0' : replicate (i `mod` 10) '0' <> showOct @Int j "") `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> j == n
          _ -> False

    it "returns BlockSize n if the value is just a positive hexadecimal number." $ do
      property . forAll (suchThat chooseAny $ \i -> i > 0) $ \i ->
        tryFrom ("0x" <> showHex i "") `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> i == n
          _ -> False

    it "returns BlockSize 0 if the value is just a positive hexadecimal number with some \"0\" as a prefix." $ do
      property . forAll (suchThat chooseAny $ \(i, j) -> i > 0 && j > 0) $ \(i, j) ->
        tryFrom (replicate (i `mod` 10) '0' <> "00x" <> showHex @Int j "") `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == 0
          _ -> False

    it "returns Nothing if the value is just a \"0\"." $ do
      tryInto @(BlockSizeMod BlockSize) "0" `shouldSatisfy` \case
        Left _ -> True
        _ -> False

    it "returns Nothing if the value is just a negative number." $ do
      property . forAll (suchThat chooseAny $ \i -> i < (0 :: Int)) $ \i ->
        tryInto @(BlockSizeMod BlockSize) (show i) `shouldSatisfy` \case
          Left _ -> True
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"K\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "K" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"k\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "k" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"M\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "M" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"m\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "m" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"G\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "G" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"g\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "g" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"T\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "T" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"t\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "t" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"P\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "P" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"p\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "p" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"E\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "E" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"e\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "e" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"Z\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "Z" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"z\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "z" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"Y\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "Y" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"y\" with any strings which don't start with \"B\"." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf $ suchThat chooseAny (/= 'B'))) $ \(i, s) ->
        tryFrom (show @Int i <> "y" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"KiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "K" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 if the suffix of the value is \"kiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "k" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"MiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "MiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 2 if the suffix of the value is \"miB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "miB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"GiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "GiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 3 if the suffix of the value is \"giB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "giB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"TiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "TiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 4 if the suffix of the value is \"tiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "tiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"PiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "PiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 5 if the suffix of the value is \"piB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "piB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"EiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "EiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 6 if the suffix of the value is \"eiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "eiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"ZiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "ZiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 7 if the suffix of the value is \"ziB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "ziB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"YiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "YiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1024 ^ 8 if the suffix of the value is \"yiB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "yiB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1024 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 if the suffix of the value is \"KB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "KB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000
          _ -> False

    it "returns BlockSize n * 1000 if the suffix of the value is \"kB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "kB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000
          _ -> False

    it "returns BlockSize n * 1000 ^ 2 if the suffix of the value is \"MB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "MB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 2 if the suffix of the value is \"mB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "mB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (2 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 3 if the suffix of the value is \"GB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "GB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 3 if the suffix of the value is \"gB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "gB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (3 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 4 if the suffix of the value is \"TB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "TB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 4 if the suffix of the value is \"tB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "tB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (4 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 5 if the suffix of the value is \"PB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "PB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 5 if the suffix of the value is \"tB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "pB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (5 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 6 if the suffix of the value is \"EB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "EB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 6 if the suffix of the value is \"eB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "eB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (6 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 7 if the suffix of the value is \"ZB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "ZB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 7 if the suffix of the value is \"zB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "zB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (7 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 8 if the suffix of the value is \"YB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "YB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (8 :: Int)
          _ -> False

    it "returns BlockSize n * 1000 ^ 8 if the suffix of the value is \"yB\" with any strings." $ do
      property . forAll (liftArbitrary2 (suchThat chooseAny $ \i -> i > 0) (listOf chooseAny)) $ \(i, s) ->
        tryFrom (show @Int i <> "yB" <> s) `shouldSatisfy` \case
          Right (NoMod (BlockSize n)) -> n == i * 1000 ^ (8 :: Int)
          _ -> False
