{-# LANGUAGE TemplateHaskell #-}

module Grammar(
    NounTemplateId, NounId, RelId, AdjTemplateId, AdjId,
    Cond, Verb,

    NounTemplate(..),
    nounTemplateId,
    nounTemplateName,
    nounTemplateRels,
    nounTemplateAdjs,

    Noun(..),
    nounId,
    nounRels,
    nounAdjs,
    nounSuper,

    Rel(..),
    relId,
    relPower,
    relVerb,
    relConds,

    Action(..),

    AdjTemplate(..),
    adjTemplateId,
    adjTemplateName,
    adjTemplateValue,

    Adj(..), 
    adjId,
    adjVal,
    adjAdjs,
    adjSuper,

    instantiateNoun, instantiateAdj
) where

import Control.Lens hiding (element)
import Text.Show.Functions

-- ID Type --
type NounTemplateId = Integer
type NounId = Integer
type RelId = Integer
type AdjTemplateId = Integer
type AdjId = Integer

-- Grammar Types --
type Cond = Noun -> Noun -> Bool
type Verb = Noun -> Noun -> [Action]

data NounTemplate = NounTemplate {
    _nounTemplateId         :: NounTemplateId,
    _nounTemplateName       :: String,
    _nounTemplateRels       :: [Rel],
    _nounTemplateAdjs       :: [Adj]
} deriving (Show)

data Noun = Noun {
    _nounId    :: NounId,
    _nounRels  :: [Rel],
    _nounAdjs  :: [Adj],
    _nounSuper :: NounTemplateId
} deriving (Show)

data Action = 
      Eats Noun Noun
    | Attacks
    | PicksUp deriving (Show)

data Rel = Rel {
    _relId    :: RelId,
    _relPower :: Double, -- Strength of relationship
    _relVerb  :: Verb,
    _relConds :: [Cond]
} deriving (Show)

data AdjTemplate = AdjTemplate {
    _adjTemplateId    :: AdjTemplateId,
    _adjTemplateName  :: String,
    _adjTemplateValue :: Integer
} deriving (Show)

data Adj = Adj {
    _adjId    :: AdjId,
    _adjVal :: Integer,
    _adjAdjs  :: [Adj],
    _adjSuper :: AdjTemplateId
} deriving (Show)

-- Lens Generation --
makeLenses ''NounTemplate
makeLenses ''Noun
makeLenses ''Rel
makeLenses ''Adj
makeLenses ''AdjTemplate

instance Eq Noun where
    x == y = x^.nounId == y^.nounId
instance Eq Adj where
    x == y = x^.adjId == y^.adjId
instance Eq Rel where
    x == y = x^.relId == y^.relId

-- Template Instantiation --
instantiateNoun :: NounTemplate -> Noun
instantiateNoun (NounTemplate ntId ntName ntRels ntAdjs) = Noun 0 ntRels ntAdjs ntId

instantiateAdj :: AdjTemplate -> Adj
instantiateAdj (AdjTemplate atId atName atVal) = Adj 0 atVal [] atId
