
{-# LANGUAGE FlexibleInstances, StandaloneDeriving, GADTs, DataKinds #-}
{-# LANGUAGE TypeOperators, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}  -- TODO: fix this by implementing arithmetic

module Language.Souffle.Experimental.Types
  ( DL(..)
  , Term(..)        -- TODO only export type
  , SimpleTerm(..)  -- TODO only export type
  , DLType(..)
  , Direction(..)
  , Context(..)
  , Name
  , VarName
  , NoVarsInFact
  ) where

import Data.Int
import Data.Kind
import Data.String
import Data.List.NonEmpty (NonEmpty)
import Type.Errors.Pretty


type Name = String
type VarName = String

data DLType = DLInt | DLString -- TODO add other primitive types
  deriving Show

data Direction = In | Out | InOut
  deriving Show

data Context
  = Definition'
  | Relation'

type family NoVarsInFact ctx :: Constraint where
  NoVarsInFact 'Relation' = ()
  NoVarsInFact _ = TypeError
    ( "You tried to use a variable in a top level fact, which is not supported in Soufflé."
    % "Possible solutions:"
    % "  - Move the fact inside a rule block."
    % "  - Replace the variable in the fact with a string, number, unsigned or float constant."
    )

-- TODO add other primitive types
data Term ctx ty where
  -- NOTE: type family is used here instead of "Atom 'Relation' ty";
  -- this allow giving a better type error in some situations.
  Var :: NoVarsInFact ctx => VarName -> Term ctx ty
  Int :: Int32 -> Term ctx Int32  -- TODO: DLInt
  Str :: String -> Term ctx String  -- TODO: DLString

instance IsString (Term ctx String) where
  fromString = Str

instance Num (Term ctx Int32) where
  fromInteger = Int . fromInteger

-- TODO turn into GADT, pass in type tag to preserve types?
data SimpleTerm
  = V VarName
  | I Int32
  | S String

data DL
  = Program [DL]
  | TypeDef VarName Direction [DLType]
  | Relation Name (NonEmpty SimpleTerm) (DL)
  | Fact Name (NonEmpty SimpleTerm)
  | And DL DL
  | Or DL DL
  | Not DL

