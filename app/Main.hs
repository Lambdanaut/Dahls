module Main where

import Grammar

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

main :: IO ()
main = return ()