
{-# LANGUAGE StandaloneDeriving, GADTs, DataKinds #-}

module Language.Souffle.Experimental.Types
  ( DL(..)
  , Atom(..)
  , DLType(..)
  , Direction(..)
  , Context(..)
  , Name
  , VarName
  ) where

import Data.Int
import Data.List.NonEmpty (NonEmpty)


type Name = String
type VarName = String

data DLType = DLInt | DLString -- TODO add other primitive types
  deriving Show

data Direction = In | Out | InOut
  deriving Show

data Context
  = Program'
  | Definition'
  | Relation'

-- TODO add other primitive types
data Atom ctx where
  Var :: VarName -> Atom 'Relation'
  Int :: Int32 -> Atom ctx
  Str :: String -> Atom ctx

data DL ctx where
  Program :: [DL 'Definition'] -> DL 'Program'
  TypeDef :: VarName -> Direction -> [DLType] -> DL 'Definition'
  Relation :: Name -> NonEmpty (Atom 'Relation') -> DL 'Relation' -> DL 'Definition'
  Fact :: Name -> NonEmpty (Atom ctx) -> DL ctx  -- TODO: make context based on atom, only allow vars
  And :: DL ctx -> DL ctx -> DL ctx
  Or :: DL ctx -> DL ctx -> DL ctx
  Not :: DL ctx -> DL ctx  -- TODO implement

