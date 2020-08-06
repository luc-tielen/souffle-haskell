
{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, DataKinds #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables, PolyKinds, ConstraintKinds #-}
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
  , underscore
  , __
  , not'
  , render
  , renderIO
  , UsageContext(..)
  , Head
  , Body
  , Structure
  , Fragment
  , CanTypeDef
  , NoVarsInAtom
  -- TODO: check if export list is complete
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Int
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Proxy
import Data.String
import GHC.Generics
import GHC.TypeLits
import Language.Souffle.Internal.Constraints (SimpleProduct)
import Language.Souffle.Class (Fact(..))
import Type.Errors.Pretty


newtype Predicate p
  = Predicate (forall f ctx. Fragment f ctx => Tuple ctx (Structure p) -> f ctx ())

type VarMap = Map VarName Int

newtype DSL ctx a = DSL (StateT VarMap (Writer [DL]) a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL], MonadState VarMap)
  via (StateT VarMap (Writer [DL]))

runDSL :: DSL 'Definition a -> DL
runDSL (DSL a) = Program $ execWriter (evalStateT a mempty)

var :: NoVarsInAtom ctx => VarName -> DSL ctx' (Term ctx ty)
var name = do
  count <- fromMaybe 0 <$> gets (Map.lookup name)
  modify $ Map.insert name (count + 1)
  let varName = if count == 0 then name else name <> "_" <> T.pack (show count)
  pure $ VarTerm varName

addDefinition :: DL -> DSL 'Definition ()
addDefinition dl = tell [dl]

data Head ctx unused
  = Head Name (NonEmpty SimpleTerm)

newtype Body ctx a = Body (Writer [DL] a)
  deriving (Functor, Applicative, Monad, MonadWriter [DL])
  via (Writer [DL])

instance Alternative (Body ctx) where
  -- TODO: consider using other operator to avoid bad implementation of empty
  empty = error "'empty' is not implemented for 'Body'"
  body1 <|> body2 = do
    let rules1 = combineRules $ runBody body1
        rules2 = combineRules $ runBody body2
    tell [Or rules1 rules2]
    pure undefined

not' :: Body ctx a -> Body ctx b
not' body = do
  let rules = combineRules $ runBody body
  tell [Not rules]
  pure undefined

runBody :: Body ctx a -> [DL]
runBody (Body m) = execWriter m

data TypeInfo (a :: k) (ts :: [Type])
  = TypeInfo

type CanTypeDef a ts =
  ( Assert (Length ts <=? 10) BigTupleError
  , ToDLTypes ts
  , ToTerms ts
  , KnownSymbols (AccessorNames a)
  , SimpleProduct a
  , Fact a
  )

typeDef :: forall a ts. (ts ~ Structure a, CanTypeDef a ts)
        => Direction -> DSL 'Definition (Predicate a)
typeDef d = do
  let p = Proxy :: Proxy a
      typeInfo = TypeInfo :: TypeInfo a ts
      name = T.pack $ factName p
      genericNames = map (("t" <>) . T.pack . show) [1..]
      accNames = maybe genericNames id $ accessorNames p
      tys = getTypes (Proxy :: Proxy ts)
      fields = map (uncurry FieldData) $ zip tys accNames
      definition = TypeDef name d fields
  addDefinition definition
  pure $ Predicate $ toFragment typeInfo name

(|-) :: Head 'Relation a -> Body 'Relation () -> DSL 'Definition ()
Head name terms |- body =
  let rules = runBody body
      relation = Rule name terms (combineRules rules)
  in addDefinition relation

combineRules :: [DL] -> DL
combineRules rules =
  if null rules
    then error "A body should consist of atleast 1 predicate."  -- TODO: fix this
    else foldl1 And rules

class Fragment f ctx where
  toFragment :: ToTerms ts => TypeInfo a ts -> Name -> Tuple ctx ts -> f ctx ()

instance Fragment Head 'Relation where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Relation) typeInfo terms
     in Head name terms'

instance Fragment Body ctx where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy ctx) typeInfo terms
    in tell [Atom name terms']

instance Fragment DSL 'Definition where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Definition) typeInfo terms
     in addDefinition $ Atom name terms'


data RenderMode = Nested | TopLevel

renderIO :: FilePath -> DL -> IO ()
renderIO path = TIO.writeFile path . render

render :: DL -> T.Text
render = flip runReader TopLevel . f where
  f = \case
    Program stmts ->
      T.unlines <$> traverse f stmts
    TypeDef name dir fields ->
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
  F x -> T.pack $ show x
  S s -> "\"" <> T.pack s <> "\""
  V v -> v
  Underscore -> "_"


type Name = T.Text
type VarName = T.Text
type AccessorName = T.Text

data DLType
  = DLNumber
  | DLUnsigned
  | DLFloat
  | DLString

data FieldData = FieldData DLType AccessorName

data Direction
  = Input
  | Output
  | InputOutput
  | Internal

data UsageContext
  = Definition
  | Relation

type family NoVarsInAtom (ctx :: UsageContext) :: Constraint where
  NoVarsInAtom ctx = Assert (ctx == 'Relation) NoVarsInAtomError

type NoVarsInAtomError =
  ( "You tried to use a variable in a top level fact, which is not supported in Soufflé."
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

instance Num (Term ctx Int32) where
  fromInteger = NumberTerm . fromInteger

instance Num (Term ctx Word32) where
  fromInteger = UnsignedTerm . fromInteger

instance Num (Term ctx Float) where
  fromInteger = FloatTerm . fromInteger

instance Fractional (Term ctx Float) where
  fromRational = FloatTerm . fromRational

data SimpleTerm
  = V VarName
  | I Int32
  | U Word32
  | F Float
  | S String
  | Underscore

data DL
  = Program [DL]
  | TypeDef VarName Direction [FieldData]
  | Rule Name (NonEmpty SimpleTerm) (DL)
  | Atom Name (NonEmpty SimpleTerm)
  | And DL DL
  | Or DL DL
  | Not DL


class ToDLTypes (ts :: [Type]) where
  getTypes :: Proxy ts -> [DLType]

instance ToDLTypes '[] where
  getTypes _ = []

instance (ToDLType t, ToDLTypes ts) => ToDLTypes (t ': ts) where
  getTypes _ = getType (Proxy :: Proxy t) : getTypes (Proxy :: Proxy ts)

class ToDLType t where
  getType :: Proxy t -> DLType

instance ToDLType Int32 where getType = const DLNumber
instance ToDLType Word32 where getType = const DLUnsigned
instance ToDLType Float where getType = const DLFloat
instance ToDLType String where getType = const DLString
instance ToDLType T.Text where getType = const DLString
instance ToDLType TL.Text where getType = const DLString

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

