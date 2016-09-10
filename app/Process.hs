{-# LANGUAGE TemplateHaskell #-}

module Process where

import Data.UUID(UUID)
import Control.Lens hiding (element)
import Control.Monad.State.Lazy(State, gets, modify)
import System.Random(StdGen, random, mkStdGen)

import Grammar

{- Process State -}
data Sim = Sim {
  _stateNouns     :: [Noun]
, _stateNounSuper :: [NounTemplate]
, _stateAdjSuper  :: [AdjTemplate]
, _stateRandGen   :: StdGen
} deriving (Show)
makeLenses ''Sim

type SimState a = State Sim a

resetProcess :: Sim 
resetProcess = Sim [] [] [] (mkStdGen 0)

{- Random Functions -}
nextUUID :: SimState UUID
nextUUID = do
  randGen <- gets (^.stateRandGen) 
  let (newId, newGen) = random randGen
  modify (set stateRandGen newGen)
  return newId