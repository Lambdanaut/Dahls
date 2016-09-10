{-# LANGUAGE TemplateHaskell #-}

module Grammar(
  NounTemplateId, NounId, RelId, AdjTemplateId, AdjId,
  Cond, Verb,

  NounTemplate(..),
  nounTemplateName,
  nounTemplateRels,
  nounTemplateAdjs,
  nounTemplateId,

  Noun(..),
  nounRels,
  nounAdjs,
  nounSuper,
  nounId,

  Rel(..),
  relPower,
  relVerb,
  relConds,
  relId,

  Action(..),

  AdjTemplate(..),
  adjTemplateName,
  adjTemplateValue,
  adjTemplateId,

  Adj(..), 
  adjVal,
  adjAdjs,
  adjSuper,
  adjId,
) where

import Control.Lens hiding (element)
import Data.UUID
import Text.Show.Functions

{- ID Type -}
type NounTemplateId = UUID
type NounId = UUID
type RelId = UUID
type AdjTemplateId = UUID
type AdjId = UUID

{- Typeclasses -}
class EqId a where 
  (===) :: a -> a -> Bool

{- Grammar Types -}
type Cond = Noun -> Noun -> Bool
type Verb = Noun -> Noun -> [Action]

data NounTemplate = NounTemplate {
  _nounTemplateName       :: String
, _nounTemplateRels       :: [Rel]
, _nounTemplateAdjs       :: [Adj]
, _nounTemplateId         :: NounTemplateId
} deriving (Show)

data Noun = Noun {
  _nounRels  :: [Rel]
, _nounAdjs  :: [Adj]
, _nounSuper :: NounTemplateId
, _nounId    :: NounId
} deriving (Show)

instance Eq Noun where
  x == y = _nounSuper x == _nounSuper y
instance EqId Noun where
  x === y = _nounId x == _nounId y

data Rel = Rel {
  _relPower :: Double -- Strength of relationship
, _relVerb  :: Verb
, _relConds :: [Cond]
, _relId    :: RelId
} deriving (Show)

instance EqId Rel where
  x === y = _relId x == _relId y

data AdjTemplate = AdjTemplate {
  _adjTemplateName  :: String
, _adjTemplateValue :: Integer
, _adjTemplateId    :: AdjTemplateId
} deriving (Show)

data Adj = Adj {
  _adjVal   :: Integer
, _adjAdjs  :: [Adj]
, _adjSuper :: AdjTemplateId
, _adjId    :: AdjId
} deriving (Show)

instance Eq Adj where
  x == y = _adjSuper x == _adjSuper y
instance EqId Adj where
  x === y = _adjId x == _adjId y

data Action = 
    ActionEats Noun Noun
  | ActionAttacks
  | ActionPicksUp deriving (Show, Eq)

{- Lens Generation -}
makeLenses ''NounTemplate
makeLenses ''Noun
makeLenses ''Rel
makeLenses ''Adj
makeLenses ''AdjTemplate
