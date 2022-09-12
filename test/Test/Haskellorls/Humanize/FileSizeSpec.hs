{-# LANGUAGE LambdaCase #-}

module Test.Haskellorls.Humanize.FileSizeSpec (spec) where

import Haskellorls.Humanize.FileSize
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = do
  describe "humanizeBI" $ do
    it "returns a NoScaled Int if 0 <= n <= 1023." $ do
      forAll (chooseInt (0, 1023)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          NoScale (ISize n') -> n == n'
          _ -> False

    it "returns a Kilo Float if 1023 < n <= 1024 * 9." $ do
      forAll (chooseInt (1024, 1024 * 9)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Kilo (DSize d) -> d >= 1 && d < 10
          _ -> False

    it "returns a Kilo Int if 1024 * 9 <= n <= 1024 * 1023." $ do
      forAll (chooseInt (1024 * 9 + 1, 1024 * 1023)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Kilo (ISize n') -> n' >= 10 && n' < 1024
          _ -> False

    it "returns a Mega Float if 1024 * 1023 < n <= 1024 ^ 2 * 9." $ do
      forAll (chooseInt (1024 * 1023 + 1, 1024 ^ (2 :: Int) * 9)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Mega (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Mega Int if 1024 ^ 2 * 9 < n < 1024 ^ 2 * 1023." $ do
      forAll (chooseInt (1024 ^ (2 :: Int) * 9 + 1, 1024 ^ (2 :: Int) * 1023)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Mega (ISize n') -> n' >= 10 && n' < 1024
          _ -> False

    it "returns a Giga Float if 1024 ^ 2 * 1023 <= n <= 1024 ^ 3 * 9." $ do
      forAll (chooseInt (1024 ^ (2 :: Int) * 1023, 1024 ^ (3 :: Int) * 9)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Giga (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Giga Int if 1024 ^ 3 * 99 < n <= 1024 ^ 3 * 1023." $ do
      forAll (chooseInt (1024 ^ (3 :: Int) * 9 + 1, 1024 ^ (3 :: Int) * 1023)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Giga (ISize n') -> n' >= 10 && n' < 1024
          _ -> False

    it "returns a Tera Float if 1024 ^ 3 * 1023 <= n <= 1024 ^ 4 * 9." $ do
      forAll (chooseInt (1024 ^ (3 :: Int) * 1023, 1024 ^ (4 :: Int) * 9)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Tera (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Tera Int if 1024 ^ 4 * 9 < n <= 1024 ^ 4 * 1023." $ do
      forAll (chooseInt (1024 ^ (4 :: Int) * 9 + 1, 1024 ^ (4 :: Int) * 1023)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Tera (ISize n') | n' >= 10 && n' < 1024 -> True
          _ -> False

    it "returns a Peta Float if 1024 ^ 4 * 1023 <= n <= 1024 ^ 5 * 9." $ do
      forAll (chooseInt (1024 ^ (4 :: Int) * 1023, 1024 ^ (5 :: Int) * 9)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Peta (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Peta Int if 1024 ^ 5 * 9 < n <= 1024 ^ 5 * 1023." $ do
      forAll (chooseInt (1024 ^ (5 :: Int) * 9 + 1, 1024 ^ (5 :: Int) * 1023)) $ \n ->
        humanizeBI n `shouldSatisfy` \case
          Peta (ISize n') -> n' >= 10 && n' < 1024
          _ -> False

  describe "humanizeSI" $ do
    it "returns a NoScale Int if 0 <= n <= 999." $ do
      forAll (chooseInt (0, 999)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          NoScale (ISize n') -> n == n'
          _ -> False

    it "returns a Kilo Float if 999 < n <= 1000 * 9." $ do
      forAll (chooseInt (1000, 1000 * 9)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Kilo (DSize d) -> d >= 1 && d < 10
          _ -> False

    it "returns a Kilo Int if 1000 * 9 < n <= 1000 * 99." $ do
      forAll (chooseInt (1000 * 9 + 1, 1000 * 999)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Kilo (ISize n') -> n' >= 10 && n' < 1000
          _ -> False

    it "returns a Mega Float if 1000 * 1000 < n <= 1000 ^ 2 * 9." $ do
      forAll (chooseInt (1000 * 999 + 1, 1000 ^ (2 :: Int) * 9)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Mega (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Mega Intif 1000 ^ 2 * 9 < n < 1000 ^ 2 * 1000." $ do
      forAll (chooseInt (1000 ^ (2 :: Int) * 9 + 1, 1000 ^ (2 :: Int) * 999)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Mega (ISize n') -> n' >= 10 && n' < 1000
          _ -> False

    it "returns a Giga Float if 1000 ^ 2 * 999 <= n <= 1000 ^ 3 * 9." $ do
      forAll (chooseInt (1000 ^ (2 :: Int) * 999, 1000 ^ (3 :: Int) * 9)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Giga (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Giga Int if 1000 ^ 3 * 9 < n <= 1000 ^ 3 * 999." $ do
      forAll (chooseInt (1000 ^ (3 :: Int) * 9 + 1, 1000 ^ (3 :: Int) * 999)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Giga (ISize n') -> n' >= 10 && n' < 1000
          _ -> False

    it "returns a Tera Float if 1000 ^ 3 * 999 <= n <= 1000 ^ 4 * 9." $ do
      forAll (chooseInt (1000 ^ (3 :: Int) * 999, 1000 ^ (4 :: Int) * 9)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Tera (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Tera Int if 1000 ^ 4 * 9 < n <= 1000 ^ 4 * 999." $ do
      forAll (chooseInt (1000 ^ (4 :: Int) * 9 + 1, 1000 ^ (4 :: Int) * 999)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Tera (ISize n') -> n' >= 10 && n' < 1024
          _ -> False

    it "returns a Peta Float if 1000 ^ 4 * 999 <= n <= 1000 ^ 5 * 9." $ do
      forAll (chooseInt (1000 ^ (4 :: Int) * 999, 1000 ^ (5 :: Int) * 9)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Peta (DSize d) -> d >= 0.1 && d < 10
          _ -> False

    it "returns a Peta Int if 1000 ^ 5 * 9 < n <= 1000 ^ 5 * 999." $ do
      forAll (chooseInt (1000 ^ (5 :: Int) * 9 + 1, 1000 ^ (5 :: Int) * 999)) $ \n ->
        humanizeSI n `shouldSatisfy` \case
          Peta (ISize n') -> n' >= 10 && n' < 1000
          _ -> False
