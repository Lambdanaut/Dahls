module MockGrammar where

import Control.Lens hiding (element)
import Data.UUID(UUID)

import Grammar
import GrammarHelpers(instantiateNoun, instantiateAdj, initObject, findAdj, condTheyAre)
import Mechanics
import Process(SimState)

-- Dummy Data
verbHungers :: Verb
verbHungers n1 n2 = [ActionEats n1 n2]

test :: SimState ()
test = do
  adjTastyTemplate          <- initObject $ AdjTemplate "tasty" 100 
  adjTasty                  <- instantiateAdj adjTastyTemplate
  adjSatietyTemplate        <- initObject $ AdjTemplate "satiety" 0
  adjSatiety                <- instantiateAdj adjSatietyTemplate
  let condHungry lowestSatiety n _ = case findAdj adjSatietyTemplate (n^.nounAdjs) of
                                        Just satiety -> satiety^.adjVal < lowestSatiety
                                        Nothing -> False
  fatGuyHungersToEatBurgers <- initObject $ Rel 100 verbHungers [(condHungry 100), (condTheyAre adjTastyTemplate)]
  hamburgerTemplate         <- initObject $ NounTemplate "hamburger" [] [adjTasty] 
  hamburger                 <- instantiateNoun hamburgerTemplate
  fatGuyTemplate            <- initObject $ NounTemplate "fat guy" [fatGuyHungersToEatBurgers] [adjSatiety]
  fatGuy                    <- instantiateNoun fatGuyTemplate

  return ()