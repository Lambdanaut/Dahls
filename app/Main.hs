{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (element)
import Data.List (find)

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
    _adjValue :: Integer,
    _adjAdjs  :: [Adj]
    _adjSuper :: AdjTemplateID,
} deriving (Show)

makeLenses ''NounTemplate
makeLenses ''Noun
makeLenses ''Rel
makeLenses ''Adj
makeLenses ''AdjTemplate

-- Test functions
verbHungers :: Verb
verbHungers n1 n2 = [Eats n1 n2]

condHungry :: Conditional
condHungry lowestSatiety n _ = case satiety of find (n^.nounAdjs) (\adj -> adj^.adjSuper == adjSatietyTemplateId) hungry of
    Just satiety -> satiety < lowestSatiety
    Nothing -> False

adjTastyTemplateId = 0
adjTastyTemplate :: AdjTemplate
adjTastyTemplate = AdjTemplate 0 "tasty" adjTastyTemplateId

adjTasty :: AdjTemplate
adjTasty = instantiate adjTastyTemplate

adjSatietyTemplateId = 1
adjSatietyTemplate :: AdjTemplate
adjSatietyTemplate = AdjTemplate 1 "satiety" adjSatietyTemplateId

adjSatiety :: Adj
adjSatiety = instantiate adjHungerTemplate

fatGuyHungersToEatBurgers :: Rel
fatGuyHungersToEatBurgers = Rel 0 100 verbHungers Wants (Just money) Nothing

hamburgerTemplate :: NounTemplate
fatGuyTemplate = NounTemplate 0 "hamburger" [] [adjMeaty] 

hamburger :: Noun
hamburger = instantiate hamburgerTemplate

fatGuyTemplate :: NounTemplate
fatGuyTemplate = NounTemplate 1 "fat guy" [fatGuyHungersToEatBurgers] [adjSatiety]

fatGuy :: Noun
fatGuy = instantiate fatGuyTemplate

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


instance Template NounTemplate where
    instantiate (NounTemplate ntId ntName ntRels ntAdjs) = Noun 0 ntName ntRels ntAdjs ntId

instance Template AdjTemplate where
    instantiate (AdjTemplate atId atName atValue) = Adj 0 atVal []

class Instantiable a where
    instantiate :: a -> b


main :: IO ()
main = return ()
