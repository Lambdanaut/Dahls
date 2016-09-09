module Mechanics where

import Control.Lens hiding (element)
import Data.List (find)

import Grammar


-- Finds an adjective in a list of adjectives that was instantiated from the template of adjTemplateId
findAdj :: AdjTemplateId -> [Adj] -> Maybe Adj
findAdj adjTemplateId adjs = find (\adj -> adj^.adjSuper == adjTemplateId) adjs

-- Helper conditions
condTheyAre :: AdjTemplateId -> Cond
condTheyAre adjTemplateId _ n2 = case findAdj adjTemplateId (n2^.nounAdjs) of
  Just _  -> True
  Nothing -> False

-- Mechanics
respond :: Noun -> Noun -> [[Action]]
respond n1 n2 = runVerbsOn passingRelationships
  where
    runVerbsOn = map (\rel -> (rel^.relVerb) n1 n2)
    passingRelationships = filter (\rel -> all (\cond -> cond n1 n2) (rel^.relConds)) (n1^.nounRels)