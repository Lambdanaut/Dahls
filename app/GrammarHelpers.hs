module GrammarHelpers where

import Control.Lens hiding (element)
import Data.List (find)
import Grammar
import Process(SimState, nextUUID)


-- Finds an adjective in a list of adjectives that was instantiated from the template of adjTemplate
findAdj :: AdjTemplate -> [Adj] -> Maybe Adj
findAdj adjTemplate adjs = find (\adj -> adj^.adjSuper == adjTemplate^.adjTemplateId) adjs

{- Helper conditions -}
condTheyAre :: AdjTemplate -> Cond
condTheyAre adjTemplate _ n2 = case findAdj adjTemplate (n2^.nounAdjs) of
  Just _  -> True
  Nothing -> False

{- Template Instantiation -}
instantiateNoun :: NounTemplate -> SimState Noun
instantiateNoun (NounTemplate ntName ntRels ntAdjs ntId) = do
  newId <- nextUUID
  return $ Noun ntRels ntAdjs ntId newId

instantiateAdj :: AdjTemplate -> SimState Adj
instantiateAdj (AdjTemplate atName atVal atId) = do
  newId <- nextUUID
  return $ Adj atVal [] atId newId 

{- Object Creation Helpers -}
-- Initializes the first parameter(The ID) of a grammar object
initObject objectConstructor = nextUUID >>= return.objectConstructor
