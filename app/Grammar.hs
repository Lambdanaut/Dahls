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

instance Eq Noun where
  x == y = _nounSuper x == _nounSuper y
instance EqId Noun where
  x === y = _nounId x == _nounId y

data Rel = Rel {
  _relId    :: RelId,
  _relPower :: Double, -- Strength of relationship
  _relVerb  :: Verb,
  _relConds :: [Cond]
} deriving (Show)

instance EqId Rel where
  x === y = _relId x == _relId y

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
