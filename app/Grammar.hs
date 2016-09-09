{-# LANGUAGE TemplateHaskell #-}

module Grammar(
    NounTemplateID, NounID, RelID, AdjTemplateID, AdjID,
    Conditional, Verb,
    NounTemplate(..),
    nounTemplateID,
    nounTemplateName,
    nounTemplateRels,
    nounTemplateAdjs,

    Noun(..),
    nounID,
    nounRels,
    nounAdjs,
    nounSuper,

    Rel(..),
    relID,
    relPower,
    relVerb,
    relConditional,

    Action(..),

    AdjTemplate(..),
    adjTemplateID,
    adjTemplateName,
    adjTemplateValue,

    Adj(..), 
    adjID,
    adjVal,
    adjAdjs,
    adjSuper,

    instantiateNoun, instantiateAdj
) where

import Control.Lens hiding (element)
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

instantiateNoun :: NounTemplate -> Noun
instantiateNoun (NounTemplate ntId ntName ntRels ntAdjs) = Noun 0 ntRels ntAdjs ntId

instantiateAdj :: AdjTemplate -> Adj
instantiateAdj (AdjTemplate atId atName atVal) = Adj 0 atVal [] atId