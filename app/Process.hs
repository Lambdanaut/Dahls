{-# LANGUAGE TemplateHaskell #-}

module Process where

import Data.UUID(UUID)
import Control.Lens hiding (element)
import Control.Monad.State.Lazy(State, gets)
import System.Random(StdGen, random, mkStdGen)

import Grammar

{- Process State -}
data Sim = Sim {
   _stateNouns     :: [Noun]
,  _stateNounSuper :: [NounTemplate]
,  _stateAdjSuper  :: [AdjTemplate]
,  _stateRandGen   :: StdGen
} deriving (Show)
makeLenses ''Sim

type SimState a = State Sim a

resetProcess :: Sim 
resetProcess = Sim [] [] [] (mkStdGen 0)

{- Template Instantiation -}
instantiateNoun :: NounTemplate -> SimState Noun
instantiateNoun (NounTemplate ntId ntName ntRels ntAdjs) = do
  randGen <- gets (^.stateRandGen) 
  let (newId, newGen) = random randGen
  -- TODO: SET THE SIM'S STDGEN TO THE NEW STDGEN HERE. 
  return $ Noun newId ntRels ntAdjs ntId

--instantiateAdj :: AdjTemplate -> SimState Adj
--instantiateAdj (AdjTemplate atId atName atVal) = return $ Adj 0 atVal [] atId