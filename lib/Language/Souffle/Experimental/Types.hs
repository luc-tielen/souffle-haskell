
{-# LANGUAGE FlexibleInstances, StandaloneDeriving, GADTs, DataKinds #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}  -- TODO: fix this by implementing arithmetic

module Language.Souffle.Experimental.Types
  ( DL(..)
  , Atom(..)        -- TODO only export type
  , SimpleAtom(..)  -- TODO only export type
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
data Atom ctx ty where
  Var :: VarName -> Atom 'Relation' ty
  Int :: Int32 -> Atom ctx Int32  -- TODO: DLInt
  Str :: String -> Atom ctx String  -- TODO: DLString

instance IsString (Atom ctx String) where
  fromString = Str

instance Num (Atom ctx Int32) where
  fromInteger = Int . fromInteger

-- TODO turn into GADT, pass in type tag to preserve types?
data SimpleAtom
  = V VarName
  | I Int32
  | S String

data DL
  = Program [DL]
  | TypeDef VarName Direction [DLType]
  | Relation Name (NonEmpty SimpleAtom) (DL)
  | Fact Name (NonEmpty SimpleAtom )
  | And DL DL
  | Or DL DL
  | Not DL    -- TODO implement

