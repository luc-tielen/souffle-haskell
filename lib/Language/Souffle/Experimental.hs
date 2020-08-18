
{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, DataKinds #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables, PolyKinds, ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-missing-methods #-} -- TODO: fix this by implementing arithmetic

module Language.Souffle.Experimental
  ( Predicate(..)
  , DSL
  , DL
  , Term
  , Direction(..)
  , runDSL
  , var
  , predicateFor
  , (|-)
  , (\/)
  , underscore
  , __
  , not'
  , (.<)
  , (.<=)
  , (.>)
  , (.>=)
  , (.=)
  , (.!=)
  , (^)
  , (%)
  , band
  , bor
  , bxor
  , lor
  , land
  , max'
  , min'
  , render
  , renderIO
  , UsageContext(..)
  , Head
  , Body
  , Structure
  , Fragment
  , ToPredicate
  , NoVarsInAtom
  -- TODO: check if export list is complete
  ) where

import Prelude hiding ((^))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Int
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Text.Printf (printf)
import Data.Proxy
import Data.String
import GHC.Generics
import GHC.TypeLits
import Language.Souffle.Internal.Constraints (SimpleProduct)
import Language.Souffle.Class (Program(..), Fact(..), ContainsFact, Direction(..))
import Type.Errors.Pretty


newtype Predicate p
  = Predicate (forall f ctx. Fragment f ctx => Tuple ctx (Structure p) -> f ctx ())

type VarMap = Map VarName Int

newtype DSL prog ctx a = DSL (StateT VarMap (Writer [AST]) a)
  deriving (Functor, Applicative, Monad, MonadWriter [AST], MonadState VarMap)
  via (StateT VarMap (Writer [AST]))


runDSL :: Program prog => prog -> DSL prog 'Definition a -> DL
runDSL _ (DSL a) = Statements $ mapMaybe simplify $ execWriter (evalStateT a mempty) where
  simplify = \case
    Declare' name dir fields -> pure $ Declare name dir fields
    Rule' name terms body -> Rule name terms <$> simplify body
    Atom' name terms -> pure $ Atom name terms
    And' exprs -> case mapMaybe simplify exprs of
      [] -> Nothing
      exprs' -> pure $ foldl1 And exprs'
    Or' exprs -> case mapMaybe simplify exprs of
      [] -> Nothing
      exprs' -> pure $ foldl1 Or exprs'
    Not' expr -> Not <$> simplify expr
    Constrain' e -> pure $ Constrain e

var :: NoVarsInAtom ctx => VarName -> DSL prog ctx' (Term ctx ty)
var name = do
  count <- fromMaybe 0 <$> gets (Map.lookup name)
  modify $ Map.insert name (count + 1)
  let varName = if count == 0 then name else name <> "_" <> T.pack (show count)
  pure $ VarTerm varName

addDefinition :: AST -> DSL prog 'Definition ()
addDefinition dl = tell [dl]

data Head ctx unused
  = Head Name (NonEmpty SimpleTerm)

newtype Body ctx a = Body (Writer [AST] a)
  deriving (Functor, Applicative, Monad, MonadWriter [AST])
  via (Writer [AST])

(\/) :: Body ctx () -> Body ctx () -> Body ctx ()
body1 \/ body2 = do
  let rules1 = And' $ runBody body1
      rules2 = And' $ runBody body2
  tell [Or' [rules1, rules2]]

not' :: Body ctx a -> Body ctx ()
not' body = do
  let rules = And' $ runBody body
  tell [Not' rules]

runBody :: Body ctx a -> [AST]
runBody (Body m) = execWriter m

data TypeInfo (a :: k) (ts :: [Type])
  = TypeInfo

type ToPredicate prog a =
  ( Fact a
  , ContainsFact prog a
  , SimpleProduct a
  , Assert (Length (Structure a) <=? 10) BigTupleError
  , KnownDLTypes (Structure a)
  , KnownDirection (FactDirection a)
  , KnownSymbols (AccessorNames a)
  , ToTerms (Structure a)
  )

predicateFor :: forall a prog. ToPredicate prog a => DSL prog 'Definition (Predicate a)
predicateFor = do
  let typeInfo = TypeInfo :: TypeInfo a (Structure a)
      p = Proxy :: Proxy a
      name = T.pack $ factName p
      accNames = fromMaybe genericNames $ accessorNames p
      genericNames = map (("t" <>) . T.pack . show) [1..]
      tys = getTypes (Proxy :: Proxy (Structure a))
      direction = getDirection (Proxy :: Proxy (FactDirection a))
      fields = zipWith FieldData tys accNames
      definition = Declare' name direction fields
  addDefinition definition
  pure $ Predicate $ toFragment typeInfo name


class KnownDirection a where
  getDirection :: Proxy a -> Direction
instance KnownDirection 'Input where getDirection = const Input
instance KnownDirection 'Output where getDirection = const Output
instance KnownDirection 'InputOutput where getDirection = const InputOutput
instance KnownDirection 'Internal where getDirection = const Internal

(|-) :: Head 'Relation a -> Body 'Relation () -> DSL prog 'Definition ()
Head name terms |- body =
  let rules = runBody body
      relation = Rule' name terms (And' rules)
  in addDefinition relation

infixl 0 |-

class Fragment f ctx where
  toFragment :: ToTerms ts => TypeInfo a ts -> Name -> Tuple ctx ts -> f ctx ()

instance Fragment Head 'Relation where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Relation) typeInfo terms
     in Head name terms'

instance Fragment Body ctx where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy ctx) typeInfo terms
    in tell [Atom' name terms']

instance Fragment (DSL prog) 'Definition where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Definition) typeInfo terms
     in addDefinition $ Atom' name terms'


data RenderMode = Nested | TopLevel

renderIO :: FilePath -> DL -> IO ()
renderIO path = TIO.writeFile path . render

render :: DL -> T.Text
render = flip runReader TopLevel . f where
  f = \case
    Statements stmts ->
      T.unlines <$> traverse f stmts
    Declare name dir fields ->
      let fieldPairs = map renderField fields
       in pure $ T.intercalate "\n" $ catMaybes
        [ Just $ ".decl " <> name <> "(" <> T.intercalate ", " fieldPairs <> ")"
        , renderDir name dir
        ]
    Atom name terms -> do
      let rendered = name <> "(" <> renderTerms (toList terms) <> ")"
      end <- maybeDot
      pure $ rendered <> end
    Rule name terms body -> do
      body' <- f body
      let rendered =
            name <> "(" <> renderTerms (toList terms) <> ") :-\n" <>
            T.intercalate "\n" (map indent $ T.lines body')
      pure rendered
    And e1 e2 -> do
      txt <- nested $ do
        txt1 <- f e1
        txt2 <- f e2
        pure $ txt1 <> ",\n" <> txt2
      end <- maybeDot
      pure $ txt <> end
    Or e1 e2 -> do
      txt <- nested $ do
        txt1 <- f e1
        txt2 <- f e2
        pure $ txt1 <> ";\n" <> txt2
      end <- maybeDot
      case end of
        "." -> pure $ txt <> end
        _ -> pure $ "(" <> txt <> ")"
    Not e -> do
      let maybeAddParens txt = case e of
            And _ _ -> "(" <> txt <> ")"
            _ -> txt
      txt <- maybeAddParens <$> nested (f e)
      end <- maybeDot
      case end of
        "." -> pure $ "!" <> txt <> end
        _ -> pure $ "!" <> txt
    Constrain t -> do
      let t' = renderTerm t
      end <- maybeDot
      case end of
        "." -> pure $ t' <> "."
        _ -> pure t'
  indent = ("  " <>)
  nested = local (const Nested)
  maybeDot = ask >>= \case
    TopLevel -> pure "."
    Nested -> pure mempty

renderDir :: VarName -> Direction -> Maybe T.Text
renderDir name = \case
  Input -> Just $ ".input " <> name
  Output -> Just $ ".output " <> name
  InputOutput -> Just $ T.intercalate "\n"
                      $ catMaybes [renderDir name Input, renderDir name Output]
  Internal -> Nothing

renderField :: FieldData -> T.Text
renderField (FieldData ty accName) =
  let txt = case ty of
        DLNumber -> ": number"
        DLUnsigned -> ": unsigned"
        DLFloat -> ": float"
        DLString -> ": symbol"
   in accName <> txt

renderTerms :: [SimpleTerm] -> T.Text
renderTerms = T.intercalate ", " . map renderTerm

renderTerm :: SimpleTerm -> T.Text
renderTerm = \case
  I x -> T.pack $ show x
  U x -> T.pack $ show x
  F x -> T.pack $ printf "%f" x
  S s -> "\"" <> T.pack s <> "\""
  V v -> v
  Underscore -> "_"

  BinOp' op t1 t2 -> renderTerm t1 <> " " <> renderBinOp op <> " " <> renderTerm t2
  UnaryOp' op t1 -> renderUnaryOp op <> renderTerm t1
  Func' name ts -> renderFunc name <> "(" <> renderTerms (toList ts) <> ")"
  where
    renderFunc = \case
      Max -> "max"
      Min -> "min"
    renderBinOp = \case
      Plus -> "+"
      Mul -> "*"
      Subtract -> "-"
      Div -> "/"
      Pow -> "^"
      Rem -> "%"
      BinaryAnd -> "band"
      BinaryOr -> "bor"
      BinaryXor -> "bxor"
      LogicalAnd -> "land"
      LogicalOr -> "lor"
      LessThan -> "<"
      LessThanOrEqual -> "<="
      GreaterThan -> ">"
      GreaterThanOrEqual -> ">="
      IsEqual -> "="
      IsNotEqual -> "!="
    renderUnaryOp Negate = "-"


type Name = T.Text
type VarName = T.Text
type AccessorName = T.Text

data DLType
  = DLNumber
  | DLUnsigned
  | DLFloat
  | DLString

data FieldData = FieldData DLType AccessorName

data UsageContext
  = Definition
  | Relation

type family NoVarsInAtom (ctx :: UsageContext) :: Constraint where
  NoVarsInAtom ctx = Assert (ctx == 'Relation) NoVarsInAtomError

type NoVarsInAtomError =
  ( "You tried to use a variable in a top level fact, which is not supported in Souffle."
  % "Possible solutions:"
  % "  - Move the fact inside a rule body."
  % "  - Replace the variable in the fact with a string, number, unsigned or float constant."
  )

data Term ctx ty where
  -- NOTE: type family is used here instead of "Atom 'Relation ty";
  -- this allows giving a better type error in some situations.
  VarTerm :: NoVarsInAtom ctx => VarName -> Term ctx ty
  UnderscoreTerm :: Term ctx ty
  NumberTerm :: Int32 -> Term ctx Int32
  UnsignedTerm :: Word32 -> Term ctx Word32
  FloatTerm :: Float -> Term ctx Float
  StringTerm :: ToString ty => ty -> Term ctx ty

  UnaryOp :: Num ty => Op1 -> Term ctx ty -> Term ctx ty
  BinOp :: Num ty => Op2 -> Term ctx ty -> Term ctx ty -> Term ctx ty
  Func :: FuncName -> NonEmpty (Term ctx ty) -> Term ctx ty

data Op2
  = Plus
  | Mul
  | Subtract
  | Div
  | Pow
  | Rem
  | BinaryAnd
  | BinaryOr
  | BinaryXor
  | LogicalAnd
  | LogicalOr
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | IsEqual
  | IsNotEqual

data Op1 = Negate

data FuncName
  = Max
  | Min


underscore, __ :: Term ctx ty
underscore = UnderscoreTerm

__ = underscore

class ToString a where
  toString :: a -> String

instance ToString String where toString = id
instance ToString T.Text where toString = T.unpack
instance ToString TL.Text where toString = TL.unpack

instance IsString (Term ctx String) where fromString = StringTerm
instance IsString (Term ctx T.Text) where fromString = StringTerm . T.pack
instance IsString (Term ctx TL.Text) where fromString = StringTerm . TL.pack

-- TODO: better name
class Num ty => SupportsArithmetic ty where
  fromInteger' :: Integer -> Term ctx ty

instance SupportsArithmetic Int32 where
  fromInteger' = NumberTerm . fromInteger
instance SupportsArithmetic Word32 where
  fromInteger' = UnsignedTerm . fromInteger
instance SupportsArithmetic Float where
  fromInteger' = FloatTerm . fromInteger

instance (SupportsArithmetic ty, Num ty) => Num (Term ctx ty) where
  fromInteger = fromInteger'
  (+) = BinOp Plus
  (*) = BinOp Mul
  (-) = BinOp Subtract
  negate = UnaryOp Negate

instance Fractional (Term ctx Float) where
  fromRational = FloatTerm . fromRational
  (/) = BinOp Div

-- TODO: avoid conflict with prelude?
(^) :: Num ty => Term ctx ty -> Term ctx ty -> Term ctx ty
(^) = BinOp Pow

(%) :: (Num ty, Integral ty) => Term ctx ty -> Term ctx ty -> Term ctx ty
(%) = BinOp Rem

type Comparison ctx ty = Num ty => Term ctx ty -> Term ctx ty -> Body ctx ()

(.<), (.<=), (.>), (.>=), (.=), (.!=) :: Comparison ctx ty
(.<) = addConstraint LessThan
(.<=) = addConstraint LessThanOrEqual
(.>) = addConstraint GreaterThan
(.>=) = addConstraint GreaterThanOrEqual
(.=) = addConstraint IsEqual
(.!=) = addConstraint IsNotEqual
infix 1 .<
infix 1 .<=
infix 1 .>
infix 1 .>=
infix 1 .=
infix 1 .!=

addConstraint :: Op2 -> Comparison ctx ty
addConstraint op e1 e2 =
  let expr = BinOp' op (toTerm e1) (toTerm e2)
   in tell [Constrain' expr]

band, bor, bxor, land, lor
  :: (Num ty, Integral ty)
  => Term ctx ty -> Term ctx ty -> Term ctx ty
band = BinOp BinaryAnd
bor = BinOp BinaryOr
bxor = BinOp BinaryXor
land = BinOp LogicalAnd
lor = BinOp LogicalOr

max', min' :: Num ty => Term ctx ty -> Term ctx ty -> Term ctx ty
max' = func2 Max
min' = func2 Min

func2 :: FuncName -> Term ctx ty -> Term ctx ty -> Term ctx ty
func2 name a b = Func name $ a :| [b]

data SimpleTerm
  = V VarName
  | I Int32
  | U Word32
  | F Float
  | S String
  | Underscore

  | BinOp' Op2 SimpleTerm SimpleTerm
  | UnaryOp' Op1 SimpleTerm
  | Func' FuncName (NonEmpty SimpleTerm)

data AST
  = Declare' VarName Direction [FieldData]
  | Rule' Name (NonEmpty SimpleTerm) AST
  | Atom' Name (NonEmpty SimpleTerm)
  | And' [AST]
  | Or' [AST]
  | Not' AST
  | Constrain' SimpleTerm

data DL
  = Statements [DL]
  | Declare VarName Direction [FieldData]
  | Rule Name (NonEmpty SimpleTerm) DL
  | Atom Name (NonEmpty SimpleTerm)
  | And DL DL
  | Or DL DL
  | Not DL
  | Constrain SimpleTerm


class KnownDLTypes (ts :: [Type]) where
  getTypes :: Proxy ts -> [DLType]

instance KnownDLTypes '[] where
  getTypes _ = []

instance (KnownDLType t, KnownDLTypes ts) => KnownDLTypes (t ': ts) where
  getTypes _ = getType (Proxy :: Proxy t) : getTypes (Proxy :: Proxy ts)

class KnownDLType t where
  getType :: Proxy t -> DLType

instance KnownDLType Int32 where getType = const DLNumber
instance KnownDLType Word32 where getType = const DLUnsigned
instance KnownDLType Float where getType = const DLFloat
instance KnownDLType String where getType = const DLString
instance KnownDLType T.Text where getType = const DLString
instance KnownDLType TL.Text where getType = const DLString

type family AccessorNames a :: [Symbol] where
  AccessorNames a = GetAccessorNames (Rep a)

type family GetAccessorNames (f :: Type -> Type) :: [Symbol] where
  GetAccessorNames (a :*: b) = GetAccessorNames a ++ GetAccessorNames b
  GetAccessorNames (C1 ('MetaCons _ _ 'False) _) = '[]
  GetAccessorNames (S1 ('MetaSel ('Just name) _ _ _) a) = '[name] ++ GetAccessorNames a
  GetAccessorNames (M1 _ _ a) = GetAccessorNames a
  GetAccessorNames (K1 _ _) = '[]

class KnownSymbols (symbols :: [Symbol]) where
  toStrings :: Proxy symbols -> [String]

instance KnownSymbols '[] where
  toStrings = const []

instance (KnownSymbol s, KnownSymbols symbols) => KnownSymbols (s ': symbols) where
  toStrings _ =
    let sym = symbolVal (Proxy :: Proxy s)
        symbols =  toStrings (Proxy :: Proxy symbols)
     in sym : symbols

accessorNames :: forall a. KnownSymbols (AccessorNames a) => Proxy a -> Maybe [T.Text]
accessorNames _ = case toStrings (Proxy :: Proxy (AccessorNames a)) of
  [] -> Nothing
  names -> Just $ T.pack <$> names

type Tuple ctx ts = TupleOf (MapType (Term ctx) ts)

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
                , toTerm g, toTerm h, toTerm i, toTerm j
                ]

toTerm :: Term ctx t -> SimpleTerm
toTerm = \case
  VarTerm v -> V v
  StringTerm s -> S $ toString s
  NumberTerm x -> I x
  UnsignedTerm x -> U x
  FloatTerm x -> F x
  UnderscoreTerm -> Underscore

  BinOp op t1 t2 -> BinOp' op (toTerm t1) (toTerm t2)
  UnaryOp op t1 -> UnaryOp' op (toTerm t1)
  Func name ts -> Func' name $ fmap toTerm ts


-- Helper functions / type families / ...

type family MapType (f :: Type -> Type) (ts :: [Type]) :: [Type] where
  MapType _ '[] = '[]
  MapType f (t ': ts) = f t ': MapType f ts

type family Assert (c :: Bool) (msg :: ErrorMessage) :: Constraint where
  Assert 'True _ = ()
  Assert 'False msg = TypeError msg

type family (a :: k) == (b :: k) :: Bool where
  a == a = 'True
  _ == _ = 'False

type family Length (xs :: [Type]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

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
  TupleOf _ = TypeError BigTupleError

type BigTupleError =
  ( "The DSL only supports facts/tuples consisting of up to 10 elements."
  % "If you need more arguments, please submit an issue on Github "
  <> "(https://github.com/luc-tielen/souffle-haskell/issues)"
  )

