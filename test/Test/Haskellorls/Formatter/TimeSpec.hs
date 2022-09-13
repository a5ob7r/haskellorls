{-# LANGUAGE BlockArguments #-}

module Test.Haskellorls.Formatter.TimeSpec (spec) where

import Data.Time
import Data.Time.Calendar.OrdinalDate
import Haskellorls.Formatter.Time
import Haskellorls.Config.TimeStyle
import Test.Syd

spec :: Spec
spec = do
  describe "formatFiletime" do
    it "formats filedate using a recent format if the fimetime is recent." do
      let zone = utc
          locale = defaultTimeLocale
          now = UTCTime (fromOrdinalDate 2020 300) 0
          filetime = UTCTime (fromOrdinalDate 2020 299) 0

      formatFiletime zone locale now FULLISO filetime `shouldBe` "2020-10-25 00:00:00.000000000000 +0000"
      formatFiletime zone locale now POSIXFULLISO filetime `shouldBe` "Oct 25 00:00"
      formatFiletime zone locale now LONGISO filetime `shouldBe` "2020-10-25 00:00"
      formatFiletime zone locale now POSIXLONGISO filetime `shouldBe` "Oct 25 00:00"
      formatFiletime zone locale now ISO filetime `shouldBe` "10-25 00:00"
      formatFiletime zone locale now POSIXISO filetime `shouldBe` "Oct 25 00:00"
      formatFiletime zone locale now LOCALE filetime `shouldBe` "Oct 25 00:00"
      formatFiletime zone locale now POSIXLOCALE filetime `shouldBe` "Oct 25 00:00"
      formatFiletime zone locale now (FORMAT []) filetime `shouldBe` ""
      formatFiletime zone locale now (FORMAT ["foo"]) filetime `shouldBe` "foo"
      formatFiletime zone locale now (FORMAT ["foo", "bar"]) filetime `shouldBe` "bar"
      formatFiletime zone locale now (FORMAT ["foo", "bar", ""]) filetime `shouldBe` "bar"

    it "formats filedate using a not-recent format if the fimetime is not-recent." do
      let zone = utc
          locale = defaultTimeLocale
          now = UTCTime (fromOrdinalDate 2020 300) 0
          filetime = UTCTime (fromOrdinalDate 2020 1) 0

      formatFiletime zone locale now FULLISO filetime `shouldBe` "2020-01-01 00:00:00.000000000000 +0000"
      formatFiletime zone locale now POSIXFULLISO filetime `shouldBe` "Jan  1  2020"
      formatFiletime zone locale now LONGISO filetime `shouldBe` "2020-01-01 00:00"
      formatFiletime zone locale now POSIXLONGISO filetime `shouldBe` "Jan  1  2020"
      formatFiletime zone locale now ISO filetime `shouldBe` "2020-01-01"
      formatFiletime zone locale now POSIXISO filetime `shouldBe` "Jan  1  2020"
      formatFiletime zone locale now LOCALE filetime `shouldBe` "Jan  1  2020"
      formatFiletime zone locale now POSIXLOCALE filetime `shouldBe` "Jan  1  2020"
      formatFiletime zone locale now (FORMAT []) filetime `shouldBe` ""
      formatFiletime zone locale now (FORMAT ["foo"]) filetime `shouldBe` "foo"
      formatFiletime zone locale now (FORMAT ["foo", "bar"]) filetime `shouldBe` "foo"
      formatFiletime zone locale now (FORMAT ["foo", "bar", ""]) filetime `shouldBe` "foo"
