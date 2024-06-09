module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Sample Test" $ do
    it "should pass" $ do
      True `shouldBe` True

