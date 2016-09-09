{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (element)
import Data.List (find)
import Text.Show.Functions

type NounTemplateID = Integer
type NounID = Integer
type RelID = Integer
type AdjTemplateID = Integer
type AdjID = Integer

type Conditional = Noun -> [Adj] -> Bool
type Verb = Noun -> Noun -> [Action]

data NounTemplate = NounTemplate {
    _nounTemplateID         :: NounTemplateID,
    _nounTemplateName       :: String,
    _nounTemplateRels       :: [Rel],
    _nounTemplateAdjs       :: [Adj]
} deriving (Show)

data Noun = Noun {
    _nounID    :: NounID,
    _nounRels  :: [Rel],
    _nounAdjs  :: [Adj],
    _nounSuper :: NounTemplateID
} deriving (Show)

data Action = 
      Eats Noun Noun
    | Attacks
    | PicksUp deriving (Show)

data Rel = Rel {
    _relID          :: RelID,
    _relPower       :: Double, -- Strength of relationship
    _relVerb        :: Verb,
    _relConditional :: [Conditional]
} deriving (Show)

data AdjTemplate = AdjTemplate {
    _adjTemplateID    :: AdjTemplateID,
    _adjTemplateName  :: String,
    _adjTemplateValue :: Integer
} deriving (Show)

data Adj = Adj {
    _adjID    :: AdjID,
    _adjVal :: Integer,
    _adjAdjs  :: [Adj],
    _adjSuper :: AdjTemplateID
} deriving (Show)

makeLenses ''NounTemplate
makeLenses ''Noun
makeLenses ''Rel
makeLenses ''Adj
makeLenses ''AdjTemplate

-- Test functions
verbHungers :: Verb
verbHungers n1 n2 = [Eats n1 n2]

condHungry :: Integer -> Conditional
condHungry lowestSatiety n _ = case find (\adj -> adj^.adjSuper == adjSatietyTemplateId) (n^.nounAdjs) of
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
fatGuyHungersToEatBurgers = Rel 0 100 verbHungers [(condHungry 100)]

hamburgerTemplate :: NounTemplate
hamburgerTemplate = NounTemplate 0 "hamburger" [] [adjTasty] 

hamburger :: Noun
hamburger = instantiateNoun hamburgerTemplate

fatGuyTemplate :: NounTemplate
fatGuyTemplate = NounTemplate 1 "fat guy" [fatGuyHungersToEatBurgers] [adjSatiety]

fatGuy :: Noun
fatGuy = instantiateNoun fatGuyTemplate

{- 
* Search for Matching Relationship
* If Matching Relationship:
    * Set Matching Relationship equal to the average of the New Relationship and the Matching Relationship
* else:
    * Add New Relationship
 -}
--addRel :: Rel -> Noun -> Noun
--addRel rel noun = find (matchingRels rel) noun^.nounRels

--matchingRels :: Rel -> Rel -> Bool
--matchingRels rel1 rel2 = rel1^.verb

instantiateNoun :: NounTemplate -> Noun
instantiateNoun (NounTemplate ntId ntName ntRels ntAdjs) = Noun 0 ntRels ntAdjs ntId

instantiateAdj :: AdjTemplate -> Adj
instantiateAdj (AdjTemplate atId atName atVal) = Adj 0 atVal [] atId


main :: IO ()
main = return ()