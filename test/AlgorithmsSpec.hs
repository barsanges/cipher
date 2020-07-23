{- |
   Module      : AlgorithmsSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Algorithms.
-}

module AlgorithmsSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck
import Algorithms

spec :: Spec
spec = do
  describe "Caesar cipher" $ do
    it "replaces ASCII capital letters (1)" $
      caesar 1 "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` "BCDEFGHIJKLMNOPQRSTUVWXYZA"

    it "replaces ASCII capital letters (2)" $
      caesar 5 "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` "FGHIJKLMNOPQRSTUVWXYZABCDE"

    it "affects only ASCII capital letters" $
      caesar 1 "Does it? IT DOES!" `shouldBe` "Eoes it? JU EPFT!"

    it "can always be inversed" $ property $
      \ i str -> (caesar' i $ caesar i str) == str

  describe "Vigenere cipher" $ do
    it "replaces ASCII capital letters" $
      vigenere "KEY" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` "LGBOJERMHUPKXSNAVQDYTGBWJE"

    it "can use a key shorter than the text" $
      vigenere "VIGENERE" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` "WKJISKYMESRQASGUMAZYIAOCUI"

    it "can use a key as long as the text" $
      vigenere "VIGENERE" "ABCDEFGH" `shouldBe` "WKJISKYM"

    it "can use a key longer than the text" $
      vigenere "VIGENERE" "ABC" `shouldBe` "WKJ"

    it "affects only ASCII capital letters" $
      vigenere "Ab C" "Does it? IT DOES!" `shouldBe` "Eoes it? IT EOEV!"

    it "can always be inversed" $ property $
      \ key str -> (vigenere' key $ vigenere key str) == str

    it "is sometimes similar to Caesar cipher" $ property $
      \ str -> vigenere "C" str == caesar 3 str

  describe "Polybius cipher" $ do
    it "replaces ASCII capital letters with 2 digit numbers" $
      polybius "ABCDEFGHIJKLMNOPQRSTUVWXYZ" `shouldBe` "0001020304051011121314152021222324253031323334354041"

    it "affects only ASCII capital letters during encryption" $
      polybius "Does it? IT DOES!" `shouldBe` "03oes it? 1231 03220430!"

    it "affects only digits during decryption" $
      polybius' "03oes it? 1231 03220430!" `shouldBe` "Does it? IT 0UM430!"

    it "can sometimes be inversed" $
      (polybius' . polybius $ "ABCDEF") == "ABCDEF" `shouldBe` True

    it "cannot always be inversed" $
      (polybius' . polybius $ "Does it? IT DOES!") == "Does it? IT DOES!" `shouldBe` False