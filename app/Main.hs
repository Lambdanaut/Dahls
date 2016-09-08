{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (element)
import Data.List (find)

data NounTemplate = DahlTemplate | ObjectTemplate deriving(Show)

type NounID = Integer

data Noun = Dahl {
    _nounID :: NounID,
    _nounName :: String,
    _nounRels :: [Rel],
    _nounClonedFrom :: NounID
} | Object {
    _nounID :: Integer,
    _nounName :: String,
    _nounClonedFrom :: NounID
} deriving(Show)

data Verb = 
      Likes
    | Wants
    | Lusts
    | Loves
    | Eats
    | Attacks
    | PicksUp
    | Owns deriving(Show)

data Conditional = Conditional deriving(Show)

data Rel = Rel {
    _relID          :: Integer,
    _relPower       :: Double, -- Strength of relationship
    _relNoun1       :: Noun,
    _relVerb        :: Verb,
    _relNoun2       :: Maybe Noun,
    _relConditional :: Maybe Conditional
} deriving(Show)

makeLenses ''Noun
makeLenses ''Rel

dahlfoo :: Noun
dahlfoo = Dahl 0 "Dahlfoo" [] DahlTemplate 

dahlbar :: Noun
dahlbar = Dahl 1 "Dahlbar" [] DahlTemplate 

money :: Noun
money = Object 2 "money" ObjectTemplate 

dahlfooWantsMoney :: Rel
dahlfooWantsMoney = Rel 0 100 dahlfoo Wants (Just money) Nothing

{- 
* Search for Matching Relationship
* If Matching Relationship:
    * Set Matching Relationship equal to the average of the New Relationship and the Matching Relationship
* else:
    * Add New Relationship
 -}
addRel :: Rel -> Noun -> Noun
addRel rel noun = find (matchingRels rel) noun^.nounRels

matchingRels :: Rel -> Rel -> Bool
matchingRels rel1 rel2 = rel1^.verb


main :: IO ()
main = return ()
