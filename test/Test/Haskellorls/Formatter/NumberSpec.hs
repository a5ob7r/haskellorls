{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Haskellorls.Formatter.NumberSpec (spec) where

import Haskellorls.Formatter.Number
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = do
  describe "formatI" do
    it "doesn't modify any number if the number is in the range from 0 to 9." do
      property . forAll (elements [0 .. 9]) $ \(i :: Int) -> formatI (Config "." "," [1] 127) i == show i

    it "doesn't modify any number if 'thousandsSep' is empty." do
      property \(i :: Int) -> formatI (Config "." "" [3] 127) i == show i

    it "doesn't modify any number if 'grouuping' is empty." do
      property \(i :: Int) -> formatI (Config "." "" [] 127) i == show i

    it "separates number digits by 'thousandsSep'." $
      formatI (Config "." ",sep," [3] 127) (1000000 :: Int) == "1,sep,000,sep,000"

    it "separates number digits until found 'charMax' in 'grouping'." $
      formatI (Config "." "," [3, 3, 127] 127) (1000000000 :: Int) == "1000,000,000"

    it "separates number digits until found a number, which is greater than 'charMax', in 'grouping'." $
      formatI (Config "." "," [3, 3, 128] 127) (1000000000 :: Int) == "1000,000,000"

    it "separates number digits into groups per elements of 'grouping' in turn." $
      formatI (Config "." "," [1, 2, 3] 127) (1000000 :: Int) == "1,000,00,0"

    it "separates number digits by the last number of 'grouping' if elements of 'grouping' is shortage to group." $
      formatI (Config "." "," [2, 1] 127) (10000 :: Int) == "1,0,0,00"

    it "separates number digits even if the number is negative." $
      formatI (Config "." "," [3] 127) (-1000000 :: Int) == "-1,000,000"

  describe "formatF" do
    it "doesn't modify any number which is infinity." $
      formatF (Config "." "," [3] 127) (1 / 0 :: Double) == "Infinity"
        && formatF (Config "." "," [3] 127) (-1 / 0 :: Double) == "-Infinity"

    it "formats a floating number using 'decimalPoint' as a decimal point." $
      formatF (Config ".point." "," [3] 127) (1.2345 :: Double) == "1.point.2345"

    it "doesn't format a part of floating number using 'thousandsSep' as a decimal point." $
      formatF (Config "." "," [3] 127) (12345.6789 :: Double) == "12,345.6789"
