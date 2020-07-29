
{-# LANGUAGE RankNTypes, GADTs, TypeFamilies, DataKinds #-}
{-# LANGUAGE TypeOperators, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, DerivingVia #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes, ScopedTypeVariables, PolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Souffle.Experimental ( main ) where

import Control.Monad.Writer
import Control.Applicative
import GHC.Generics
import GHC.TypeLits
import Data.Kind
import Data.Char
import Data.Proxy
import Data.List.NonEmpty

type family a ++ b = c where
  '[] ++ b = b
  a ++ '[] = a
  (a ': b) ++ c = a ': (b ++ c)

type family Structure a :: [Type] where
  Structure a = Collect (Rep a)

type family Collect (a :: Type -> Type) where
  Collect (a :*: b) = Collect a ++ Collect b
  Collect (M1 _ _ a) = Collect a
  Collect (K1 _ ty) = '[ty]


nameFor :: forall a. (KnownSymbol (NameFor a)) => VarName
nameFor = formatName $ symbolVal (Proxy @(NameFor a)) where
  formatName = fmap toLower -- TODO camelcase to snake case?

type family NameFor a where
  NameFor a = GetName (Rep a)

type family GetName (repr :: Type -> Type) :: Symbol where
  GetName (D1 ('MetaData name _ _ _) _) = name


class GetDLTypes (a :: [Type]) where
  getTypes :: [DLType]

instance GetDLTypes '[] where
  getTypes = []

instance (GetDLType x, GetDLTypes xs) => GetDLTypes (x ': xs) where
  getTypes = getType @x : getTypes @xs

class GetDLType a where
  getType :: DLType

instance GetDLType Int where
  getType = DLInt

instance GetDLType String where
  getType = DLString


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

combineRules :: [DL] -> DL
combineRules rules =
  if null rules
    then error "A block should consist of atleast 1 predicate."
    else foldl1 And rules

(|-) :: Head -> Block () -> DSL ()
Head name atoms |- block =
  let rules = runBlock block
      relation = Relation name atoms (combineRules rules)
  in addDefinition relation


class Fragment f where
  toFragment :: Name -> NonEmpty Atom -> f

instance Fragment Head where
  toFragment = Head

instance ty ~ () => Fragment (Block ty) where
  toFragment name atoms = tell [Fact name atoms]

instance ty ~ () => Fragment (DSL ty) where
  toFragment name atoms = addDefinition $ Fact name atoms

type family MapConstraint (c :: Type -> Constraint) (xs :: [Type]) :: Constraint where
  MapConstraint _ '[] = ()
  MapConstraint c (x ': xs) = (c x, MapConstraint c xs)

type family TupleOf (ts :: [Type]) where
  TupleOf '[t] = t
  TupleOf '[t1, t2] = (t1, t2)
  TupleOf '[t1, t2, t3] = (t1, t2, t3)
  TupleOf '[t1, t2, t3, t4] = (t1, t2, t3, t4)
  TupleOf '[t1, t2, t3, t4, t5] = (t1, t2, t3, t4, t5)
  TupleOf '[t1, t2, t3, t4, t5, t6] = (t1, t2, t3, t4, t5, t6)
  TupleOf '[t1, t2, t3, t4, t5, t6, t7] = (t1, t2, t3, t4, t5, t6, t7)
  TupleOf '[t1, t2, t3, t4, t5, t6, t7, t8] = (t1, t2, t3, t4, t5, t6, t7, t8)
  TupleOf '[t1, t2, t3, t4, t5, t6, t7, t8, t9] = (t1, t2, t3, t4, t5, t6, t7, t8, t9)
  TupleOf '[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10] = (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
  -- Only facts with up to 10 arguments are currently supported.

class ToAtoms (ts :: [Type]) where
  toAtoms :: TypeInfo a ts -> TupleOf ts -> NonEmpty Atom

instance MapConstraint ToAtom '[t] => ToAtoms '[t] where
  toAtoms _ a = toAtom a :| []
instance MapConstraint ToAtom [t1, t2] => ToAtoms [t1, t2] where
  toAtoms _ (a, b) = toAtom a :| [toAtom b]
instance MapConstraint ToAtom [t1, t2, t3] => ToAtoms [t1, t2, t3] where
  toAtoms _ (a, b, c) = toAtom a :| [toAtom b, toAtom c]
instance MapConstraint ToAtom [t1, t2, t3, t4] => ToAtoms [t1, t2, t3, t4] where
  toAtoms _ (a, b, c, d) = toAtom a :| [toAtom b, toAtom c, toAtom d]
instance MapConstraint ToAtom [t1, t2, t3, t4, t5] => ToAtoms [t1, t2, t3, t4, t5] where
  toAtoms _ (a, b, c, d, e) = toAtom a :| [toAtom b, toAtom c, toAtom d, toAtom e]
instance MapConstraint ToAtom [t1, t2, t3, t4, t5, t6] => ToAtoms [t1, t2, t3, t4, t5, t6] where
  toAtoms _ (a, b, c, d, e, f) = toAtom a :| [toAtom b, toAtom c, toAtom d, toAtom e, toAtom f]
instance MapConstraint ToAtom [t1, t2, t3, t4, t5, t6, t7]
  => ToAtoms [t1, t2, t3, t4, t5, t6, t7] where
  toAtoms _ (a, b, c, d, e, f, g) =
    toAtom a :| [toAtom b, toAtom c, toAtom d, toAtom e, toAtom f, toAtom g]
instance MapConstraint ToAtom [t1, t2, t3, t4, t5, t6, t7, t8]
  => ToAtoms [t1, t2, t3, t4, t5, t6, t7, t8] where
  toAtoms _ (a, b, c, d, e, f, g, h) =
    toAtom a :| [toAtom b, toAtom c, toAtom d, toAtom e, toAtom f, toAtom g, toAtom h]
instance MapConstraint ToAtom [t1, t2, t3, t4, t5, t6, t7, t8, t9]
  => ToAtoms [t1, t2, t3, t4, t5, t6, t7, t8, t9] where
  toAtoms _ (a, b, c, d, e, f, g, h, i) =
    toAtom a :| [toAtom b, toAtom c, toAtom d, toAtom e, toAtom f, toAtom g, toAtom h, toAtom i]
instance MapConstraint ToAtom [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10]
  => ToAtoms [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10] where
  toAtoms _ (a, b, c, d, e, f, g, h, i, j) =
    toAtom a :| [ toAtom b, toAtom c, toAtom d, toAtom e, toAtom f
                , toAtom g, toAtom h, toAtom i, toAtom j ]
  -- Only facts with up to 10 arguments are currently supported.

class ToAtom a where
  toAtom :: a -> Atom

instance ToAtom Int where toAtom = Int
instance ToAtom String where toAtom = Str

type Name = String
type VarName = String

data DLType = DLInt | DLString -- TODO other primitive types
  deriving Show

data Atom = Int Int | Str String | Var VarName
  deriving Show

data Direction = In | Out | InOut
  deriving Show

data TypeInfo (a :: k) (ts :: [Type])
  = TypeInfo

data DL
  = Program [DL]
  | TypeDef VarName Direction [DLType]
  | Fact Name (NonEmpty Atom)  -- TODO only allow primitive types, no vars
  | Relation Name (NonEmpty Atom) DL
  | And DL DL
  | Or DL DL
  | Not DL   -- TODO implement
  deriving Show

typeDef :: forall a ts. ts ~ Structure a
        => GetDLTypes ts
        => ToAtoms ts
        => KnownSymbol (GetName (Rep a))
        => Direction
        -> DSL (Predicate ts)
typeDef d = do
  let name = nameFor @a
      definition = TypeDef name d (getTypes @ts)
      typeInfo :: TypeInfo a ts
      typeInfo = TypeInfo
  addDefinition definition
  pure $ Predicate $ toFragment name . toAtoms typeInfo

newtype Predicate ts = Predicate (forall f. Fragment f => TupleOf ts -> f)


data Edge = Edge String String
  deriving Generic

data Reachable = Reachable String String
  deriving Generic

main :: IO ()
main = do
  let program = runDSL $ do
        Predicate edge <- typeDef @Edge In
        Predicate reachable <- typeDef @Edge In

        reachable("a", "b") |- do
          edge("a", "b")
        reachable ("a", "b") |- do
          edge("a", "c")
          reachable("c", "b")

  print program
