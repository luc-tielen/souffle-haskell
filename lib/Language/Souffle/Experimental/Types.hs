
{-# LANGUAGE FlexibleInstances, StandaloneDeriving, GADTs, DataKinds #-}
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
  ) where

import Data.Int
import Data.String
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
data Term ctx ty where
  Var :: VarName -> Term 'Relation' ty
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

