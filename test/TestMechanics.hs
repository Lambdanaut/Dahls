module TestMechanics where

import Test.Hspec

import Grammar
import Mechanics(findAdj, respond)

import MockGrammar

test :: IO ()
test = hspec $ do
  describe "Mechanics.findAdj" $ do
    it "should return `Just Adjective` when the templateID is found" $ do
      findAdj adjSatietyTemplateId [adjTasty, adjSatiety] `shouldBe` (Just adjSatiety :: Maybe Adj)

    it "should return `Nothing` when the templateID isn't found" $
      findAdj 987654321 [adjTasty, adjSatiety] `shouldBe` (Nothing :: Maybe Adj)

  describe "Mechanics.interact" $ do
    it "Should return lists of lists of actions after a passing condition" $
      (fatGuy `respond` hamburger) `shouldBe` ([[ActionEats fatGuy hamburger]] :: [[Action]])