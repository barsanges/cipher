{- |
   Module      : UtilsSpec
   Copyright   : Copyright (C) 2020 barsanges
   License     : GNU GPL, version 3

Test the module Utils.
-}

module UtilsSpec ( spec ) where

import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "lighten" $ do
    it "inserts one white space every 'n' characters" $
      lighten 4 "abcdef" `shouldBe` "abcd ef"

    it "does not insert a whitespace at the end of the string" $
      lighten 4 "abcdefgh" `shouldBe` "abcd efgh"

    it "does nothing if the string is too short" $
      lighten 100 "abcdefgh" `shouldBe` "abcdefgh"

  describe "simplify" $ do
    it "an English pangram" $
      simplify "The quick brown fox jumps over the lazy dog." `shouldBe` "foo?"

    it "a French pangram" $
      simplify "Portez ce vieux whisky au juge blond qui fume." `shouldBe` "PORTEZCEVIEUXWHISKYAUJUGEBLONDQUIFUME."

    it "a French pangram with all French characters" $
      simplify "Portez ce vieux whisky au juge blond qui fume sur son île\
               \ intérieure, à côté de l'alcôve ovoïde, où les bûches se\
               \ consument dans l'âtre, ce qui lui permet de penser à la\
               \ cænogenèse de l'être dont il est question dans la cause\
               \ ambiguë entendue à Moÿ, dans un capharnaüm qui, pense-t-il,\
               \ diminue çà et là la qualité de son œuvre." `shouldBe`
               "PORTEZCEVIEUXWHISKYAUJUGEBLONDQUIFUMESURSONILE\
               \INTERIEURE,ACOTEDEL'ALCOVEOVOIDE,OULESBUCHESSE\
               \CONSUMENTDANSL'ATRE,CEQUILUIPERMETDEPENSERALA\
               \CAENOGENESEDEL'ETREDONTILESTQUESTIONDANSLACAUSE\
               \AMBIGUEENTENDUEAMOY,DANSUNCAPHARNAUMQUI,PENSE-T-IL,\
               \DIMINUECAETLALAQUALITEDESONOEUVRE."

    it "another French pangram with all French characters" $
      simplify "Dès Noël, où un zéphyr haï me vêt de glaçons würmiens, je dîne\
               \ d'exquis rôtis de bœuf au kir, à l'Aÿ d'âge mûr, et cætera."
               `shouldBe` "DESNOEL,OUUNZEPHYRHAIMEVETDEGLACONSWURMIENS,JEDINE\
               \D'EXQUISROTISDEBOEUFAUKIR,AL'AYD'AGEMUR,ETCAETERA."