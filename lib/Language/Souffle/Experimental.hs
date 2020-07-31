
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
  , render

  , Fragment
  , GetDLTypes
  , ToAtoms
  ) where

import Language.Souffle.Experimental.Internal
import Control.Monad.Writer
import Control.Applicative
import GHC.TypeLits
import Data.Kind
import Data.Int
import qualified Data.Text as T
import Data.Proxy
import Data.List.NonEmpty (NonEmpty(..), toList)


newtype Predicate p ts
  = Predicate (forall f. Fragment f => TupleOf (Structure p) -> f)

type Name = String
type VarName = String

data DLType = DLInt | DLString -- TODO other primitive types
  deriving Show

data Atom = Int Int32 | Str String | Var VarName
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

instance ToAtom Int32 where toAtom = Int
instance ToAtom String where toAtom = Str

render :: DL -> T.Text
render = \case
  Program stmts ->
    T.unlines $ fmap render stmts
  TypeDef name dir ts ->
    let pairs = zip [1..] ts
        nameTypePairs = map (uncurry renderType) pairs
    in T.intercalate "\n"
      [ ".decl " <> T.pack name <> "(" <> T.intercalate ", " nameTypePairs <> ")"
      , renderDir name dir
      ]
  Fact name atoms ->
    T.pack name <> "(" <> renderAtoms (toList atoms) <> ")."
  Relation name atoms body ->
    T.pack name <> "(" <> renderAtoms (toList atoms) <> ") :-\n" <>
      T.intercalate "\n" (map indent $ T.lines (render body))
  And _ _ -> ""
  Or _ _ -> ""
  Not _ -> ""

indent :: T.Text -> T.Text
indent = ("  " <>)

renderDir :: VarName -> Direction -> T.Text
renderDir name = \case
  In -> ".input " <> T.pack name
  Out -> ".output " <> T.pack name
  InOut -> T.intercalate "\n" [renderDir name In, renderDir name Out]

renderType :: Int -> DLType -> T.Text
renderType x = \case
  DLInt -> x' <> ": number"
  DLString -> x' <> ": symbol"
  where x' = "t" <> T.pack (show x)

renderAtoms :: [Atom] -> T.Text
renderAtoms = T.intercalate ", " . fmap renderAtom

renderAtom :: Atom -> T.Text
renderAtom = \case
  Int x -> T.pack $ show x
  Str s -> "\"" <> T.pack s <> "\""
  Var v -> T.pack v
