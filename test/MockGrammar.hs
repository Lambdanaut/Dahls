module MockGrammar (
    verbHungers,
    condHungry,
    adjSatietyTemplateId,
    adjTastyTemplate,
    adjTasty,
    adjSatietyTemplateId,
    adjSatietyTemplate,
    adjSatiety,
    fatGuyHungersToEatBurgers,
    hamburgerTemplate,
    hamburger,
    fatGuyTemplate,
    fatGuy,
) where

import Control.Lens hiding (element)
import Data.List (find)

import Test.HUnit

import Grammar
import Mechanics

-- Dummy Data
verbHungers :: Verb
verbHungers n1 n2 = [Eats n1 n2]

condHungry :: Integer -> Cond
condHungry lowestSatiety n _ = case findAdj adjSatietyTemplateId (n^.nounAdjs) of
    Just satiety -> satiety^.adjVal < lowestSatiety
    Nothing -> False

adjTastyTemplateId = 0
adjTastyTemplate :: AdjTemplate
adjTastyTemplate = AdjTemplate 0 "tasty" adjTastyTemplateId

adjTasty :: Adj
adjTasty = instantiateAdj adjTastyTemplate

adjSatietyTemplateId = 1
adjSatietyTemplate :: AdjTemplate
adjSatietyTemplate = AdjTemplate 1 "satiety" adjSatietyTemplateId

adjSatiety :: Adj
adjSatiety = instantiateAdj adjSatietyTemplate

fatGuyHungersToEatBurgers :: Rel
fatGuyHungersToEatBurgers = Rel 0 100 verbHungers [(condHungry 100), (condTheyAre adjTastyTemplateId)]

hamburgerTemplate :: NounTemplate
hamburgerTemplate = NounTemplate 0 "hamburger" [] [adjTasty] 

hamburger :: Noun
hamburger = instantiateNoun hamburgerTemplate

fatGuyTemplate :: NounTemplate
fatGuyTemplate = NounTemplate 1 "fat guy" [fatGuyHungersToEatBurgers] [adjSatiety]

fatGuy :: Noun
fatGuy = instantiateNoun fatGuyTemplate