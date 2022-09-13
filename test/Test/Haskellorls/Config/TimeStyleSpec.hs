{-# LANGUAGE BlockArguments #-}

module Test.Haskellorls.Config.TimeStyleSpec (spec) where

import Haskellorls.Config.TimeStyle
import Haskellorls.System.Locale
import Test.Syd
import Witch (From (..))

spec :: Spec
spec = do
  describe "From TimeStyle with LcTime to TimeStyle" do
    it "strips POSIX if it's on non POSIX locale." do
      from (POSIXFULLISO, LcTime Nothing) `shouldBe` FULLISO
      from (POSIXFULLISO, LcTime $ Just "en_US.UTF-8") `shouldBe` FULLISO

      from (POSIXLONGISO, LcTime Nothing) `shouldBe` LONGISO
      from (POSIXLONGISO, LcTime $ Just "en_US.UTF-8") `shouldBe` LONGISO

      from (POSIXISO, LcTime Nothing) `shouldBe` ISO
      from (POSIXISO, LcTime $ Just "en_US.UTF-8") `shouldBe` ISO

      from (POSIXLOCALE, LcTime Nothing) `shouldBe` LOCALE
      from (POSIXLOCALE, LcTime $ Just "en_US.UTF-8") `shouldBe` LOCALE

    it "doesn't modify any style if it's on POSIX locale." do
      from (FULLISO, LcTime $ Just "C") `shouldBe` FULLISO
      from (FULLISO, LcTime $ Just "POSIX") `shouldBe` FULLISO

      from (POSIXFULLISO, LcTime $ Just "C") `shouldBe` POSIXFULLISO
      from (POSIXFULLISO, LcTime $ Just "POSIX") `shouldBe` POSIXFULLISO

      from (LONGISO, LcTime $ Just "C") `shouldBe` LONGISO
      from (LONGISO, LcTime $ Just "POSIX") `shouldBe` LONGISO

      from (POSIXLONGISO, LcTime $ Just "C") `shouldBe` POSIXLONGISO
      from (POSIXLONGISO, LcTime $ Just "POSIX") `shouldBe` POSIXLONGISO

      from (ISO, LcTime $ Just "C") `shouldBe` ISO
      from (ISO, LcTime $ Just "POSIX") `shouldBe` ISO

      from (POSIXISO, LcTime $ Just "C") `shouldBe` POSIXISO
      from (POSIXISO, LcTime $ Just "POSIX") `shouldBe` POSIXISO

      from (LOCALE, LcTime $ Just "C") `shouldBe` LOCALE
      from (LOCALE, LcTime $ Just "POSIX") `shouldBe` LOCALE

      from (POSIXLOCALE, LcTime $ Just "C") `shouldBe` POSIXLOCALE
      from (POSIXLOCALE, LcTime $ Just "POSIX") `shouldBe` POSIXLOCALE

      from (FORMAT [], LcTime $ Just "C") `shouldBe` FORMAT []
      from (FORMAT [], LcTime $ Just "POSIX") `shouldBe` FORMAT []
