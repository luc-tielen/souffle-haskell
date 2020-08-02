
{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, DataKinds, TypeApplications #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables, PolyKinds, InstanceSigs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-missing-methods #-} -- TODO: fix this by implementing arithmetic

module Language.Souffle.Experimental
  ( Predicate(..)
  , DSL
  , DL
  , Direction(..)
  , runDSL
  , var
  , typeDef
  , (|-)
  , not
  , render
  , renderIO
  , Context(..)
  , Head
  , Block
  , Fragment
  , GetDLTypes
  , ToTerms
  , Structure
  ) where

import Prelude hiding (not)
import qualified Language.Souffle.Class as S (Fact(..))
import qualified Language.Souffle.Internal.Constraints as S (SimpleProduct)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Int
import Data.Proxy
import Data.List.NonEmpty (NonEmpty(..), toList)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map ( Map )
import GHC.Generics
import GHC.TypeLits
import Data.String
import Type.Errors.Pretty


newtype Predicate p
  = Predicate (forall f ctx. Fragment f ctx
              => TupleOf (MapType (Term ctx) (Structure p)) -> f ctx ())

type VarMap = Map VarName Int

newtype DSL ctx a = DSL (StateT VarMap (Writer [DL]) a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL], MonadState VarMap)
  via (StateT VarMap (Writer [DL]))

runDSL :: DSL 'Definition' a -> DL
runDSL (DSL a) = Program $ execWriter (evalStateT a mempty)

var :: NoVarsInFact ctx => VarName -> DSL ctx' (Term ctx ty)
var name = do
  count <- fromMaybe 0 <$> gets (Map.lookup name)
  modify $ Map.insert name (count + 1)
  let varName = if count == 0 then name else name <> "_" <> show count
  pure $ Var varName

addDefinition :: DL -> DSL 'Definition' ()
addDefinition dl = tell [dl]

data Head ctx unused
  = Head Name (NonEmpty SimpleTerm)

newtype Block ctx a = Block (Writer [DL] a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL])
  via (Writer [DL])

instance Alternative (Block ctx) where
  -- TODO: consider using other operator to avoid bad implementation of empty
  empty = error "'empty' is not implemented for 'Block'"
  block1 <|> block2 = do
    let rules1 = combineRules $ runBlock block1
        rules2 = combineRules $ runBlock block2
    tell [Or rules1 rules2]
    pure undefined

not :: Block ctx a -> Block ctx b
not block = do
  let rules = combineRules $ runBlock block
  tell [Not rules]
  pure undefined

runBlock :: Block ctx a -> [DL]
runBlock (Block m) = execWriter m


typeDef :: forall a ts. ts ~ Structure a
        => GetDLTypes ts
        => GetNames (AccessorNames a)
        => ToTerms ts
        => S.Fact a  -- TODO remove need for qualified import
        => S.SimpleProduct a (Rep a)  -- TODO remove need for (Rep a)
        => Direction
        -> DSL 'Definition' (Predicate a)
typeDef d = do
  let p :: Proxy a
      p = Proxy
      name = S.factName p
      genericNames = map (("t" <>) . show) [1..]
      accNames = maybe genericNames id $ accessorNames p
      tys = getTypes (Proxy :: Proxy ts)
      fields = map (uncurry FieldData) $ zip tys accNames
      definition = TypeDef name d fields
      typeInfo :: TypeInfo a ts
      typeInfo = TypeInfo
  addDefinition definition
  pure $ Predicate $ toFragment typeInfo name

(|-) :: Head 'Relation' a -> Block 'Relation' () -> DSL 'Definition' ()
Head name terms |- block =
  let rules = runBlock block
      relation = Relation name terms (combineRules rules)
  in addDefinition relation

combineRules :: [DL] -> DL
combineRules rules =
  if null rules
    then error "A block should consist of atleast 1 predicate."
    else foldl1 And rules

class Fragment f ctx where
  toFragment :: ToTerms ts => TypeInfo a ts -> Name -> Tuple ctx ts -> f ctx ()

instance Fragment Head 'Relation' where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Relation') typeInfo terms
     in Head name terms'

instance Fragment Block ctx where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy ctx) typeInfo terms
    in tell [Fact name terms']

instance Fragment DSL 'Definition' where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Definition') typeInfo terms
     in addDefinition $ Fact name terms'


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

data TypeInfo (a :: k) (ts :: [Type])
  = TypeInfo

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


type family AccessorNames a :: [Symbol] where
  AccessorNames a = GetAccessorNames (Rep a)

type family GetAccessorNames (f :: Type -> Type) :: [Symbol] where
  GetAccessorNames (a :*: b) = GetAccessorNames a ++ GetAccessorNames b
  GetAccessorNames (C1 ('MetaCons _ _ 'False) _) = '[]
  GetAccessorNames (S1 ('MetaSel ('Just name) _ _ _) a) = '[name] ++ GetAccessorNames a
  GetAccessorNames (M1 _ _ a) = GetAccessorNames a
  GetAccessorNames (K1 _ _) = '[]

-- TODO: think of better name
class GetNames (symbols :: [Symbol]) where
  getNames :: Proxy symbols -> [String]

instance GetNames '[] where
  getNames = const []

instance (KnownSymbol s, GetNames symbols) => GetNames (s ': symbols) where
  getNames _ =
    let sym = symbolVal (Proxy :: Proxy s)
        symbols =  getNames (Proxy :: Proxy symbols)
     in sym : symbols

accessorNames :: forall a. GetNames (AccessorNames a) => Proxy a -> Maybe [String]
accessorNames _ = case getNames (Proxy :: Proxy (AccessorNames a)) of
  [] -> Nothing
  names -> Just names

type family MapType (f :: Type -> Type) (ts :: [Type]) :: [Type] where
  MapType _ '[] = '[]
  MapType f (t ': ts) = f t ': MapType f ts

type Tuple ctx ts = TupleOf (MapType (Term ctx) ts)

type family TupleOf (ts :: [Type]) = t where
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
  -- NOTE: Only facts with up to 10 arguments are currently supported.

class ToTerms (ts :: [Type]) where
  toTerms :: Proxy ctx -> TypeInfo a ts -> Tuple ctx ts -> NonEmpty SimpleTerm

instance ToTerms '[t] where
  toTerms _ _ a =
    toTerm a :| []

instance ToTerms '[t1, t2] where
  toTerms _ _ (a, b) =
    toTerm a :| [toTerm b]

instance ToTerms '[t1, t2, t3] where
  toTerms _ _ (a, b, c) =
    toTerm a :| [toTerm b, toTerm c]

instance ToTerms '[t1, t2, t3, t4] where
  toTerms _ _ (a, b, c, d) =
    toTerm a :| [toTerm b, toTerm c, toTerm d]

instance ToTerms '[t1, t2, t3, t4, t5] where
  toTerms _ _ (a, b, c, d, e) =
    toTerm a :| [toTerm b, toTerm c, toTerm d, toTerm e]

instance ToTerms '[t1, t2, t3, t4, t5, t6] where
  toTerms _ _ (a, b, c, d, e, f) =
    toTerm a :| [toTerm b, toTerm c, toTerm d, toTerm e, toTerm f]

instance ToTerms '[t1, t2, t3, t4, t5, t6, t7] where
  toTerms _ _ (a, b, c, d, e, f, g) =
    toTerm a :| [toTerm b, toTerm c, toTerm d, toTerm e, toTerm f, toTerm g]

instance ToTerms '[t1, t2, t3, t4, t5, t6, t7, t8] where
  toTerms _ _ (a, b, c, d, e, f, g, h) =
    toTerm a :| [toTerm b, toTerm c, toTerm d, toTerm e, toTerm f, toTerm g, toTerm h]

instance ToTerms '[t1, t2, t3, t4, t5, t6, t7, t8, t9] where
  toTerms _ _ (a, b, c, d, e, f, g, h, i) =
    toTerm a :| [toTerm b, toTerm c, toTerm d, toTerm e, toTerm f, toTerm g, toTerm h, toTerm i]

instance ToTerms '[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10] where
  toTerms _ _ (a, b, c, d, e, f, g, h, i, j) =
    toTerm a :| [ toTerm b, toTerm c, toTerm d, toTerm e, toTerm f
                , toTerm g, toTerm h, toTerm i, toTerm j ]
-- NOTE: Only facts with up to 10 arguments are currently supported.

toTerm :: Term ctx t -> SimpleTerm
toTerm = \case
  Var v -> V v
  Str s -> S s
  Int x -> I x



data RenderMode = Nested | TopLevel

renderIO :: FilePath -> DL -> IO ()
renderIO path = TIO.writeFile path . render

render :: DL -> T.Text
render = flip runReader TopLevel . f where
  f = \case
    Program stmts -> do
      T.unlines <$> traverse f stmts
    TypeDef name dir fields ->
      let fieldPairs = map renderField fields
       in pure $ T.intercalate "\n"
        [ ".decl " <> T.pack name <> "(" <> T.intercalate ", " fieldPairs <> ")"
        , renderDir name dir
        ]
    Fact name terms -> do
      let rendered = T.pack name <> "(" <> renderTerms (toList terms) <> ")"
      end <- maybeDot
      pure $ rendered <> end
    Relation name terms body -> do
      body' <- f body
      let rendered =
            T.pack name <> "(" <> renderTerms (toList terms) <> ") :-\n" <>
            T.intercalate "\n" (map indent $ T.lines body')
      pure rendered
    And e1 e2 -> do
      txt <- local (const Nested) $ do
        txt1 <- f e1
        txt2 <- f e2
        pure $ txt1 <> ",\n" <> txt2
      end <- maybeDot
      pure $ txt <> end
    Or e1 e2 -> do
      txt <- local (const Nested) $ do
        txt1 <- f e1
        txt2 <- f e2
        pure $ txt1 <> ";\n" <> txt2
      end <- maybeDot
      case end of
        "." -> pure $ txt <> end
        _ -> pure $ "(" <> txt <> ")"
    Not e -> do
      -- TODO: refactor, should be handled in and?
      let maybeAddParens txt = case e of
            And _ _ -> "(" <> txt <> ")"
            _ -> txt
      txt <- maybeAddParens <$> local (const Nested) (f e)
      end <- maybeDot
      case end of
        "." -> pure $ "!" <> txt <> end
        _ -> pure $ "!" <> txt

  maybeDot = ask >>= \case
    TopLevel -> pure "."
    Nested -> pure mempty

indent :: T.Text -> T.Text
indent = ("  " <>)

renderDir :: VarName -> Direction -> T.Text
renderDir name = \case
  In -> ".input " <> T.pack name
  Out -> ".output " <> T.pack name
  InOut -> T.intercalate "\n" [renderDir name In, renderDir name Out]

renderField :: FieldData -> T.Text
renderField (FieldData ty accName) =
  let txt1 = T.pack accName
      txt2 = case ty of
        DLInt -> ": number"
        DLString -> ": symbol"
   in txt1 <> txt2

renderTerms :: [SimpleTerm] -> T.Text
renderTerms = T.intercalate ", " . fmap renderTerm

renderTerm :: SimpleTerm -> T.Text
renderTerm = \case
  I x -> T.pack $ show x
  S s -> "\"" <> T.pack s <> "\""
  V v -> T.pack v


type Name = String
type VarName = String
type AccessorName = String

data DLType = DLInt | DLString -- TODO add other primitive types
  deriving Show

data FieldData = FieldData DLType AccessorName

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
  | TypeDef VarName Direction [FieldData]
  | Relation Name (NonEmpty SimpleTerm) (DL)
  | Fact Name (NonEmpty SimpleTerm)
  | And DL DL
  | Or DL DL
  | Not DL

