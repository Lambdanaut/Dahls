module TestMechanics where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Grammar
import Mechanics

import MockGrammar

test :: IO ()
test = hspec $ do
  describe "Mechanics.findAdj" $ do
    it "should return `Just Adjective` when the templateID is found" $ do
      findAdj adjSatietyTemplateId [adjTasty, adjSatiety] `shouldBe` (Just adjSatiety :: Maybe Adj)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException