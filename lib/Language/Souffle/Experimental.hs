
{-# LANGUAGE RankNTypes, TypeFamilies, DataKinds #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables, PolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Souffle.Experimental
  ( Predicate(..)
  , DSL
  , DL
  , Direction(..)
  , runDSL
  , typeDef
  , (|-)
  , Head
  , Block
  , Fragment
  , GetDLTypes
  , ToAtoms
  ) where

import Language.Souffle.Experimental.Internal
import Language.Souffle.Experimental.Types
import Control.Monad.Writer
import Control.Applicative
import GHC.TypeLits
import Data.Kind
import Data.Int
import Data.Proxy
import Data.List.NonEmpty (NonEmpty(..))


newtype Predicate p ts
  = Predicate (forall f. Fragment f => TupleOf (Structure p) -> f)

newtype DSL a = DSL (Writer [DL] a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL])
  via (Writer [DL])

runDSL :: DSL a -> DL
runDSL (DSL a) = Program $ execWriter a

addDefinition :: DL -> DSL ()
addDefinition dl = tell [dl]

data Head = Head Name (NonEmpty Atom)

newtype Block a = Block (Writer [DL] a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL])
  via (Writer [DL])

instance Alternative Block where
  empty = error "'empty' is not implemented for 'Block'"
  block1 <|> block2 = do
    let rules1 = combineRules $ runBlock block1
        rules2 = combineRules $ runBlock block2
    tell [Or rules1 rules2]
    pure undefined

runBlock :: Block a -> [DL]
runBlock (Block m) = execWriter m

typeDef :: forall a ts. ts ~ Structure a
        => GetDLTypes ts
        => ToAtoms ts
        => KnownSymbol (NameFor a)
        => Direction
        -> DSL (Predicate a ts)
typeDef d = do
  let name = nameFor (Proxy :: Proxy a)
      definition = TypeDef name d (getTypes (Proxy :: Proxy ts))
      typeInfo :: TypeInfo a ts
      typeInfo = TypeInfo
  addDefinition definition
  pure $ Predicate $ toFragment name . toAtoms typeInfo

(|-) :: Head -> Block () -> DSL ()
Head name atoms |- block =
  let rules = runBlock block
      relation = Relation name atoms (combineRules rules)
  in addDefinition relation

combineRules :: [DL] -> DL
combineRules rules =
  if null rules
    then error "A block should consist of atleast 1 predicate."
    else foldl1 And rules

class Fragment f where
  toFragment :: Name -> NonEmpty Atom -> f

instance Fragment Head where
  toFragment = Head

instance ty ~ () => Fragment (Block ty) where
  toFragment name atoms = tell [Fact name atoms]

instance ty ~ () => Fragment (DSL ty) where
  toFragment name atoms = addDefinition $ Fact name atoms


class GetDLTypes (ts :: [Type]) where
  getTypes :: Proxy ts -> [DLType]

instance GetDLTypes '[] where
  getTypes _ = []

instance (GetDLType t, GetDLTypes ts) => GetDLTypes (t ': ts) where
  getTypes _ = getType (Proxy :: Proxy t) : getTypes (Proxy :: Proxy ts)

class GetDLType t where
  getType :: Proxy t -> DLType

instance GetDLType Int32 where getType = const DLInt
instance GetDLType String where getType = const DLString


