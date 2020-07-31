
module Language.Souffle.Experimental.Types
  ( DL(..)
  , Atom(..)
  , DLType(..)
  , Direction(..)
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

data Atom = Int Int32 | Str String | Var VarName
  deriving Show

data DL
  = Program [DL]
  | TypeDef VarName Direction [DLType]
  | Fact Name (NonEmpty Atom)  -- TODO only allow primitive types, no vars
  | Relation Name (NonEmpty Atom) DL
  | And DL DL
  | Or DL DL
  | Not DL   -- TODO implement
  deriving Show

