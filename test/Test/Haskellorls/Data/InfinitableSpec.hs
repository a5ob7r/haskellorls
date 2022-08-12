module Test.Haskellorls.Data.InfinitableSpec (spec) where

import Haskellorls.Data.Infinitable
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = do
  describe "Only a" $ do
    it "is always smaller than Infinity." $ do
      property $ \x -> Only x < (Infinity :: Infinitable Int)

    it "returns the same result with capseled values comparison if compare with other Only a." $ do
      property $ \(x, y) -> Only x `compare` Only y == x `compare` (y :: Int)

  describe "Infinity" $ do
    it "returns True if compare with itself" $
      Infinity == (Infinity :: Infinitable Int)
