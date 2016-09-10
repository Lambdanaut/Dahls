module Mechanics where

import Control.Lens hiding (element)

import Grammar

respond :: Noun -> Noun -> [[Action]]
respond n1 n2 = runVerbsOn passingRelationships
  where
    runVerbsOn = map (\rel -> (rel^.relVerb) n1 n2)
    passingRelationships = filter (\rel -> all (\cond -> cond n1 n2) (rel^.relConds)) (n1^.nounRels)