
{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, DataKinds #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables, PolyKinds, InstanceSigs #-}
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


newtype Predicate p
  = Predicate (forall f ctx. Fragment f ctx
              => TupleOf (MapType (Atom ctx) (Structure p)) -> f ctx ())

newtype DSL ctx a = DSL (Writer [DL ctx] a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL ctx])
  via (Writer [DL ctx])

runDSL :: DSL 'Definition' a -> DL 'Program'
runDSL (DSL a) = Program $ execWriter a

addDefinition :: DL 'Definition' -> DSL 'Definition' ()
addDefinition dl = tell [dl]

data Head ctx unused where
  Head :: Name -> NonEmpty SimpleAtom -> Head 'Relation' ()

newtype Block ctx a = Block (Writer [DL ctx] a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL ctx])
  via (Writer [DL ctx])

instance Alternative (Block ctx) where
  empty = error "'empty' is not implemented for 'Block'"
  block1 <|> block2 = do
    let rules1 = combineRules $ runBlock block1
        rules2 = combineRules $ runBlock block2
    tell [Or rules1 rules2]
    pure undefined

runBlock :: Block ctx a -> [DL ctx]
runBlock (Block m) = execWriter m

typeDef :: forall a ts. ts ~ Structure a
        => GetDLTypes ts
        => ToAtoms ts
        => KnownSymbol (NameFor a)
        => Direction
        -> DSL 'Definition' (Predicate a)
typeDef d = do
  let name = nameFor (Proxy :: Proxy a)
      definition = TypeDef name d (getTypes (Proxy :: Proxy ts))
      typeInfo :: TypeInfo a ts
      typeInfo = TypeInfo
  addDefinition definition
  pure $ Predicate $ toFragment typeInfo name

(|-) :: Head 'Relation' a -> Block 'Relation' () -> DSL 'Definition' ()
Head name atoms |- block =
  let rules = runBlock block
      relation = Relation name atoms (combineRules rules)
  in addDefinition relation

combineRules :: [DL ctx] -> DL ctx
combineRules rules =
  if null rules
    then error "A block should consist of atleast 1 predicate."
    else foldl1 And rules

class Fragment f ctx where
  toFragment :: ToAtoms ts => TypeInfo a ts -> Name -> Tuple ctx ts -> f ctx ()

instance Fragment Head 'Relation' where
  toFragment typeInfo name atoms =
    let atoms' = toAtoms (Proxy :: Proxy 'Relation') typeInfo atoms
     in Head name atoms'

instance Fragment Block ctx where
  toFragment typeInfo name atoms =
    let atoms' = toAtoms (Proxy :: Proxy ctx) typeInfo atoms -- toAtoms typeInfo (Proxy :: Proxy ctx) atoms
    in tell [Fact name atoms']

instance Fragment DSL 'Definition' where
  toFragment typeInfo name atoms =
    let atoms' = toAtoms (Proxy :: Proxy 'Definition') typeInfo atoms
     in addDefinition $ Fact name atoms'


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

