{-# LANGUAGE GADTs, RankNTypes, DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DerivingVia, ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds, TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-| This module provides an experimental DSL for generating Souffle Datalog code,
    directly from Haskell.

    The module is meant to be imported unqualified, unlike the rest of this
    library. This allows for a syntax that is very close to the corresponding
    Datalog syntax you would normally write.

    The functions and operators provided by this module follow a naming scheme:

    - If there is no clash with something imported via Prelude, the
      function or operator is named exactly the same as in Souffle.
    - If there is a clash for functions, an apostrophe is appended
      (e.g. "max" in Datalog is 'max'' in Haskell).
    - Most operators (besides those from the Num typeclass) start with a "."
      (e.g. '.^' is the "^"  operator in Datalog)

    The DSL makes heavy use of Haskell's typesystem to avoid
    many kinds of errors. This being said, not everything can be checked at
    compile-time (for example performing comparisons on ungrounded variables
    can't be checked). For this reason you should regularly write the
    Datalog code to a file while prototyping your algorithm and check it using
    the Souffle executable for errors.

    A large subset of the Souffle language is covered, with some exceptions
    such as "$", aggregates, ... There are no special functions for supporting
    components either, but this is automatically possible by making use of
    polymorphism in Haskell.

    Here's an example snippet of Haskell code that can generate Datalog code:

    @
    -- Assuming we have 2 types of facts named Edge and Reachable:
    data Edge = Edge String String
    data Reachable = Reachable String String

    program = do
      Predicate edge <- predicateFor \@Edge
      Predicate reachable <- predicateFor \@Reachable
      a <- var "a"
      b <- var "b"
      c <- var "c"
      reachable(a, b) |- edge(a, b)
      reachable(a, b) |- do
        edge(a, c)
        reachable(c, b)
    @

    When rendered to a file (using 'renderIO'), this generates the following
    Souffle code:

    @
    .decl edge(t1: symbol, t2: symbol)
    .input edge
    .decl reachable(t1: symbol, t2: symbol)
    .output reachable
    reachable(a, b) :-
      edge(a, b)
    reachable(a, b) :- do
      edge(a, c)
      reachable(c, b)
    @

    For more examples, take a look at the <https://github.com/luc-tielen/souffle-haskell/blob/2c24e1e169da269c45fc192ab5efd4ff2196114b/tests/Test/Language/Souffle/ExperimentalSpec.hs tests>.
-}
module Language.Souffle.Experimental
  ( -- * DSL-related types and functions
    -- ** Types
    Predicate(..)
  , Fragment
  , Tuple
  , DSL
  , Head
  , Body
  , Term
  , VarName
  , UsageContext(..)
  , Direction(..)
  , ToPredicate
  , FactMetadata(..)
  , Metadata(..)
  , StructureOpt(..)
  , InlineOpt(..)
  -- ** Basic building blocks
  , predicateFor
  , var
  , __
  , underscore
  , (|-)
  , (\/)
  , not'
  -- ** Souffle operators
  , (.<)
  , (.<=)
  , (.>)
  , (.>=)
  , (.=)
  , (.!=)
  , (.^)
  , (.%)
  , band
  , bor
  , bxor
  , lor
  , land
  -- ** Souffle functions
  , max'
  , min'
  , cat
  , contains
  , match
  , ord
  , strlen
  , substr
  , to_number
  , to_string
  -- * Functions for running a Datalog DSL fragment / AST directly.
  , runSouffleInterpretedWith
  , runSouffleInterpreted
  , embedProgram
  -- * Rendering functions
  , render
  , renderIO
  -- * Helper type families useful in some situations
  , Structure
  , NoVarsInAtom
  , SupportsArithmetic
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Int
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Data.Proxy
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Word
import GHC.Generics
import GHC.TypeLits
import Language.Haskell.TH.Syntax (qRunIO, qAddForeignFilePath, Q, Dec, ForeignSrcLang(..))
import Language.Souffle.Class ( Program(..), Fact(..), ContainsFact, Direction(..) )
import Language.Souffle.Internal.Constraints (SimpleProduct)
import qualified Language.Souffle.Interpreted as I
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process
import Text.Printf (printf)
import Type.Errors.Pretty


-- | A datatype that contains a function for generating Datalog AST fragments
--   that can be glued together using other functions in this module.
--
--   The rank-N type allows using the inner function in multiple places to
--   generate different parts of the AST. This is one of the key things
--   that allows writing Haskell code in a very smilar way to the Datalog code.
--
--   The inner function uses the 'Structure' of a type to compute what the
--   shape of the input tuple for the predicate should be. For example, if a
--   fact has a data constructor containing a Float and a String,
--   the resulting tuple will be of type ('Term' ctx Float, 'Term' ctx String).
--
--   Currently, only facts with up to 10 fields are supported. If you need more
--   fields, please file an issue on
--   <https://github.com/luc-tielen/souffle-haskell/issues Github>.
newtype Predicate a
  = Predicate (forall f ctx. Fragment f ctx => Tuple ctx (Structure a) -> f ctx ())

type VarMap = Map VarName Int

-- | The main monad in which Datalog AST fragments are combined together
--   using other functions in this module.
--
--   - The "prog" type variable is used for performing many compile time checks.
--     This variable is filled (automatically) with a type that implements the
--     'Program' typeclass.
--   - The "ctx" type variable is the context in which a DSL fragment is used.
--     For more information, see 'UsageContext'.
--   - The "a" type variable is the value contained inside
--     (just like other monads).
newtype DSL prog ctx a = DSL (StateT VarMap (Writer [AST]) a)
  deriving (Functor, Applicative, Monad, MonadWriter [AST], MonadState VarMap)
  via (StateT VarMap (Writer [AST]))

addDefinition :: AST -> DSL prog 'Definition ()
addDefinition dl = tell [dl]

-- | This function runs the DSL fragment directly using the souffle interpreter
--   executable.
--
--   It does this by saving the fragment to a temporary file right before
--   running the souffle interpreter. All created files are automatically
--   cleaned up after the souffle related actions have been executed. If this is
--   not your intended behavior, see 'runSouffleInterpretedWith' which allows
--   passing in different interpreter settings.
runSouffleInterpreted
  :: (MonadIO m, Program prog)
  => prog
  -> DSL prog 'Definition ()
  -> (Maybe (I.Handle prog) -> I.SouffleM a)
  -> m a
runSouffleInterpreted program dsl f = liftIO $ do
  tmpDir <- getCanonicalTemporaryDirectory
  souffleHsDir <- createTempDirectory tmpDir "souffle-haskell"
  defaultCfg <- I.defaultConfig
  let cfg = defaultCfg { I.cfgDatalogDir = souffleHsDir
                       , I.cfgFactDir = Just souffleHsDir
                       , I.cfgOutputDir = Just souffleHsDir
                       }
  runSouffleInterpretedWith cfg program dsl f <* removeDirectoryRecursive souffleHsDir

-- | This function runs the DSL fragment directly using the souffle interpreter
--   executable.
--
--   It does this by saving the fragment to a file in the directory specified by
--   the 'I.cfgDatalogDir' field in the interpreter settings. Depending on the
--   chosen settings, the fact and output files may not be automatically cleaned
--   up after running the souffle interpreter. See 'I.runSouffleWith' for more
--   information on automatic cleanup.
runSouffleInterpretedWith
  :: (MonadIO m, Program prog)
  => I.Config
  -> prog
  -> DSL prog 'Definition ()
  -> (Maybe (I.Handle prog) -> I.SouffleM a)
  -> m a
runSouffleInterpretedWith config program dsl f = liftIO $ do
  let progName = programName program
      datalogFile = I.cfgDatalogDir config </> progName <.> "dl"
  renderIO program datalogFile dsl
  I.runSouffleWith config program f

-- | Embeds a Datalog program from a DSL fragment directly in a Haskell file.
--
--   Note that due to TemplateHaskell staging restrictions, this function must
--   be used in a different module than the module where 'Program' and 'Fact'
--   instances are defined.
--
--   In order to use this function correctly, you have to add the following
--   line to the top of the module where 'embedProgram' is used in order
--   for the embedded C++ code to be compiled correctly:
--
--   > {-# OPTIONS_GHC -optc-std=c++17 -D__EMBEDDED_SOUFFLE__ #-}
embedProgram :: Program prog => prog -> DSL prog 'Definition () -> Q [Dec]
embedProgram program dsl = do
  cppFile <- qRunIO $ do
    tmpDir <- getCanonicalTemporaryDirectory
    souffleHsDir <- createTempDirectory tmpDir "souffle-haskell"
    let progName = programName program
        datalogFile = souffleHsDir </> progName <.> "dl"
        cppFile = souffleHsDir </> progName <.> "cpp"
    renderIO program datalogFile dsl
    callCommand $ printf "souffle -g %s %s" cppFile datalogFile
    pure cppFile
  qAddForeignFilePath LangCxx cppFile
  pure []

runDSL :: Program prog => prog -> DSL prog 'Definition a -> DL
runDSL _ (DSL a) = Statements $ mapMaybe simplify $ execWriter (evalStateT a mempty) where
  simplify = \case
    Declare' name dir fields opts -> pure $ Declare name dir fields opts
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

-- | Generates a unique variable, using the name argument as a hint.
--
--   The type of the variable is determined the first predicate it is used in.
--   The 'NoVarsInAtom' constraint generates a user-friendly type error if the
--   generated variable is used inside a relation (which is not valid in
--   Datalog).
--
--   Note: If a variable is created but not used using this function, you will
--   get a compile-time error because it can't deduce the constraint.
var :: NoVarsInAtom ctx => VarName -> DSL prog ctx' (Term ctx ty)
var name = do
  count <- gets (fromMaybe 0 . Map.lookup name)
  modify $ Map.insert name (count + 1)
  let varName = if count == 0 then name else name <> "_" <> T.pack (show count)
  pure $ VarTerm varName

-- | Data type representing the head of a relation
--   (the part before ":-" in a Datalog relation).
--
--   - The "ctx" type variable is the context in which this type is used.
--     For this type, this will always be 'Relation'. The variable is there to
--     perform some compile-time checks.
--   - The "unused" type variable is unused and only there so the type has the
--     same kind as 'Body' and 'DSL'.
--
--   See also '|-'.
data Head ctx unused
  = Head Name (NonEmpty SimpleTerm)

-- | Data type representing the body of a relation
--   (what follows after ":-" in a Datalog relation).
--
--   By being a monad, it supports do-notation which allows for a syntax
--   that is quite close to Datalog.
--
--   - The "ctx" type variable is the context in which this type is used.
--     For this type, this will always be 'Relation'. The variable is there to
--     perform some compile-time checks.
--   - The "a" type variable is the value contained inside
--     (just like other monads).
--
--   See also '|-'.
newtype Body ctx a = Body (Writer [AST] a)
  deriving (Functor, Applicative, Monad, MonadWriter [AST])
  via (Writer [AST])

-- | Creates a fragment that is the logical disjunction (OR) of 2 sub-fragments.
--   This corresponds with ";" in Datalog.
(\/) :: Body ctx () -> Body ctx () -> Body ctx ()
body1 \/ body2 = do
  let rules1 = And' $ runBody body1
      rules2 = And' $ runBody body2
  tell [Or' [rules1, rules2]]

-- | Creates a fragment that is the logical negation of a sub-fragment.
--   This is equivalent to "!" in Datalog. (But this operator can't be used
--   in Haskell since it only allows unary negation as a prefix operator.)
not' :: Body ctx a -> Body ctx ()
not' body = do
  let rules = And' $ runBody body
  tell [Not' rules]

runBody :: Body ctx a -> [AST]
runBody (Body m) = execWriter m

data TypeInfo (a :: k) (ts :: [Type]) = TypeInfo

-- | Constraint that makes sure a type can be converted to a predicate function.
--   It gives a user-friendly error in case any of the sub-constraints
--   are not met.
type ToPredicate prog a =
  ( Fact a
  , FactMetadata a
  , ContainsFact prog a
  , SimpleProduct a
  , Assert (Length (Structure a) <=? 10) BigTupleError
  , KnownDLTypes (Structure a)
  , KnownDirection (FactDirection a)
  , KnownSymbols (AccessorNames a)
  , ToTerms (Structure a)
  )

-- | A typeclass for optionally configuring extra settings
--   (for performance reasons).

--   Since it contains no required functions, it is possible to derive this
--   typeclass automatically (this gives you the default behavior):
--
--   @
--   data Edge = Edge String String
--     deriving (Generic, Marshal, FactMetadata)
--   @
class (Fact a, SimpleProduct a) => FactMetadata a where
  -- | An optional function for configuring fact metadata.
  --
  --   By default no extra options are configured.
  --   For more information, see the 'Metadata' type.
  factOpts :: Proxy a -> Metadata a
  factOpts = const $ Metadata Automatic NoInline

-- | A data type that allows for finetuning of fact settings
--   (for performance reasons).
data Metadata a
  = Metadata (StructureOpt a) (InlineOpt (FactDirection a))

-- | Datatype describing the way a fact is stored inside Datalog.
--   A different choice of storage type can lead to an improvement in
--   performance (potentially).
--
--   For more information, see this
--   <https://souffle-lang.github.io/tuning#datastructure link> and this
--   <https://souffle-lang.github.io/relations link>.
data StructureOpt (a :: Type) where
  -- | Automatically choose the underlying storage for a relation.
  --   This is the storage type that is used by default.
  --
  --   For Souffle, it will choose a direct btree for facts with arity <= 6.
  --   For larger facts, it will use an indirect btree.
  Automatic :: StructureOpt a
  -- | Uses a direct btree structure.
  BTree :: StructureOpt a
  -- | Uses a brie structure. This can improve performance in some cases and is
  --   more memory efficient for particularly large relations.
  Brie :: StructureOpt a
  -- | A high performance datastructure optimised specifically for equivalence
  --   relations. This is only valid for binary facts with 2 fields of the
  --   same type.
  EqRel :: (IsBinaryRelation a, Structure a ~ '[t, t]) => StructureOpt a

type IsBinaryRelation a =
  Assert (Length (Structure a) == 2)
         ("Equivalence relations are only allowed with binary relations" <> ".")

-- | Datatype indicating if we should inline a fact or not.
data InlineOpt (d :: Direction) where
  -- | Inlines the fact, only possible for internal facts.
  Inline :: InlineOpt 'Internal
  -- | Does not inline the fact.
  NoInline :: InlineOpt d

-- | Generates a function for a type that implements 'Fact' and is a
--   'SimpleProduct'. The predicate function takes the same amount of arguments
--   as the original fact type. Calling the function with a tuple of arguments,
--   creates fragments of datalog code that can be glued together using other
--   functions in this module.
--
--   Note: You need to specify for which fact you want to return a predicate
--   for using TypeApplications.
predicateFor :: forall a prog. ToPredicate prog a => DSL prog 'Definition (Predicate a)
predicateFor = do
  let typeInfo = TypeInfo :: TypeInfo a (Structure a)
      p = Proxy :: Proxy a
      name = T.pack $ factName p
      accNames = fromMaybe genericNames $ accessorNames p
      opts = toSimpleMetadata $ factOpts p
      genericNames = map (("t" <>) . T.pack . show) [1..]
      tys = getTypes (Proxy :: Proxy (Structure a))
      direction = getDirection (Proxy :: Proxy (FactDirection a))
      fields = zipWith FieldData tys accNames
      definition = Declare' name direction fields opts
  addDefinition definition
  pure $ Predicate $ toFragment typeInfo name

toSimpleMetadata :: Metadata a -> SimpleMetadata
toSimpleMetadata (Metadata struct inline) =
  let structOpt = case struct of
        Automatic -> AutomaticLayout
        BTree -> BTreeLayout
        Brie -> BrieLayout
        EqRel -> EqRelLayout
      inlineOpt = case inline of
        Inline -> DoInline
        NoInline -> DoNotInline
  in SimpleMetadata structOpt inlineOpt

class KnownDirection a where
  getDirection :: Proxy a -> Direction
instance KnownDirection 'Input where getDirection = const Input
instance KnownDirection 'Output where getDirection = const Output
instance KnownDirection 'InputOutput where getDirection = const InputOutput
instance KnownDirection 'Internal where getDirection = const Internal

-- | Turnstile operator from Datalog, used in relations.
--
--   This is used for creating a DSL fragment that contains a relation.
--   NOTE: |- is used instead of :- due to limitations of the Haskell syntax.
(|-) :: Head 'Relation a -> Body 'Relation () -> DSL prog 'Definition ()
Head name terms |- body =
  let rules = runBody body
      relation = Rule' name terms (And' rules)
  in addDefinition relation

infixl 0 |-

-- | A typeclass used for generating AST fragments of Datalog code.
--   The generated fragments can be further glued together using the
--   various functions in this module.
class Fragment f ctx where
  toFragment :: ToTerms ts => TypeInfo a ts -> Name -> Tuple ctx ts -> f ctx ()

instance Fragment Head 'Relation where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Relation) typeInfo terms
     in Head name terms'

instance Fragment Body 'Relation where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Relation) typeInfo terms
    in tell [Atom' name terms']

instance Fragment (DSL prog) 'Definition where
  toFragment typeInfo name terms =
    let terms' = toTerms (Proxy :: Proxy 'Definition) typeInfo terms
     in addDefinition $ Atom' name terms'


data RenderMode = Nested | TopLevel

-- | Renders a DSL fragment to the corresponding Datalog code and writes it to
--   a file.
renderIO :: Program prog => prog -> FilePath -> DSL prog 'Definition () -> IO ()
renderIO prog path = TIO.writeFile path . render prog

-- | Renders a DSL fragment to the corresponding Datalog code.
render :: Program prog => prog -> DSL prog 'Definition () -> T.Text
render prog = flip runReader TopLevel . f . runDSL prog where
  f = \case
    Statements stmts ->
      T.unlines <$> traverse f stmts
    Declare name dir fields metadata ->
      let fieldPairs = map renderField fields
          renderedFactOpts = renderMetadata metadata
          renderedOpts = if T.null renderedFactOpts then "" else " " <> renderedFactOpts
       in pure $ T.intercalate "\n" $ catMaybes
        [ Just $ ".decl " <> name <> "(" <> T.intercalate ", " fieldPairs <> ")" <> renderedOpts
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

renderMetadata :: SimpleMetadata -> T.Text
renderMetadata (SimpleMetadata struct inline) =
  let structTxt = case struct of
        AutomaticLayout -> Nothing
        BTreeLayout -> Just "btree"
        BrieLayout -> Just "brie"
        EqRelLayout -> Just "eqrel"
      inlineTxt = case inline of
        DoInline -> Just "inline"
        DoNotInline -> Nothing
  in T.intercalate " " $ catMaybes [structTxt, inlineTxt]

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
      Cat -> "cat"
      Contains -> "contains"
      Match -> "match"
      Ord -> "ord"
      StrLen -> "strlen"
      Substr -> "substr"
      ToNumber -> "to_number"
      ToString -> "to_string"
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

-- | Type representing a variable name in Datalog.
type VarName = T.Text

type AccessorName = T.Text

data DLType
  = DLNumber
  | DLUnsigned
  | DLFloat
  | DLString

data FieldData = FieldData DLType AccessorName

-- | A type level tag describing in which context a DSL fragment is used.
--   This is only used on the type level and helps catch some semantic errors
--   at compile time.
data UsageContext
  = Definition
  -- ^ A DSL fragment is used in a top level definition.
  | Relation
  -- ^ A DSL fragment is used inside a relation (either head or body of a relation).

-- | A type family used for generating a user-friendly type error in case
--   you use a variable in a DSL fragment where it is not allowed
--   (outside of relations).
type family NoVarsInAtom (ctx :: UsageContext) :: Constraint where
  NoVarsInAtom ctx = Assert (ctx == 'Relation) NoVarsInAtomError

type NoVarsInAtomError =
  ( "You tried to use a variable in a top level fact, which is not supported in Souffle."
  % "Possible solutions:"
  % "  - Move the fact inside a rule body."
  % "  - Replace the variable in the fact with a string, number, unsigned or float constant."
  )

-- | Data type for representing Datalog terms.
--
--   All constructors are hidden, but with the `Num`, 'Fractional' and
--   `IsString` instances it is possible to create terms using Haskell syntax
--   for literals. For non-literal values, smart constructors are provided.
--   (See for example 'underscore' / '__'.)
data Term ctx ty where
  -- NOTE: type family is used here instead of "Term 'Relation ty";
  -- this allows giving a better type error in some situations.
  VarTerm :: NoVarsInAtom ctx => VarName -> Term ctx ty
  UnderscoreTerm :: Term ctx ty
  NumberTerm :: Int32 -> Term ctx Int32
  UnsignedTerm :: Word32 -> Term ctx Word32
  FloatTerm :: Float -> Term ctx Float
  StringTerm :: ToString ty => ty -> Term ctx ty

  UnaryOp :: Num ty => Op1 -> Term ctx ty -> Term ctx ty
  BinOp :: Num ty => Op2 -> Term ctx ty -> Term ctx ty -> Term ctx ty
  Func :: FuncName -> NonEmpty SimpleTerm -> Term ctx ty2

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
  | Cat
  | Contains
  | Match
  | Ord
  | StrLen
  | Substr
  | ToNumber
  | ToString


-- | Term representing a wildcard ("_") in Datalog.
underscore :: Term ctx ty
underscore = UnderscoreTerm

-- | Term representing a wildcard ("_") in Datalog. Note that in the DSL this
--   is with 2 underscores. (Single underscore is reserved for typed holes!)
__ :: Term ctx ty
__ = underscore

class ToString a where
  toString :: a -> String

instance ToString String where toString = id
instance ToString T.Text where toString = T.unpack
instance ToString TL.Text where toString = TL.unpack

instance IsString (Term ctx String) where fromString = StringTerm
instance IsString (Term ctx T.Text) where fromString = StringTerm . T.pack
instance IsString (Term ctx TL.Text) where fromString = StringTerm . TL.pack

-- | A helper typeclass, mainly used for avoiding a lot of boilerplate
--   in the 'Num' instance for 'Term'.
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
  abs = error "'abs' is not supported for Souffle terms"
  signum = error "'signum' is not supported for Souffle terms"

instance Fractional (Term ctx Float) where
  fromRational = FloatTerm . fromRational
  (/) = BinOp Div

-- | Exponentiation operator ("^" in Datalog).
(.^) :: Num ty => Term ctx ty -> Term ctx ty -> Term ctx ty
(.^) = BinOp Pow

-- | Remainder operator ("%" in Datalog).
(.%) :: (Num ty, Integral ty) => Term ctx ty -> Term ctx ty -> Term ctx ty
(.%) = BinOp Rem

-- | Creates a less than constraint (a < b), for use in the body of a relation.
(.<) :: Num ty => Term ctx ty -> Term ctx ty -> Body ctx ()
(.<) = addConstraint LessThan
infix 1 .<

-- | Creates a less than or equal constraint (a <= b), for use in the body of
--   a relation.
(.<=) :: Num ty => Term ctx ty -> Term ctx ty -> Body ctx ()
(.<=) = addConstraint LessThanOrEqual
infix 1 .<=

-- | Creates a greater than constraint (a > b), for use in the body of a relation.
(.>) :: Num ty => Term ctx ty -> Term ctx ty -> Body ctx ()
(.>) = addConstraint GreaterThan
infix 1 .>

-- | Creates a greater than or equal constraint (a >= b), for use in the body of
--   a relation.
(.>=) :: Num ty => Term ctx ty -> Term ctx ty -> Body ctx ()
(.>=) = addConstraint GreaterThanOrEqual
infix 1 .>=

-- | Creates a constraint that 2 terms should be equal to each other (a = b),
--   for use in the body of a relation.
(.=) :: Term ctx ty -> Term ctx ty -> Body ctx ()
(.=) = addConstraint IsEqual
infix 1 .=

-- | Creates a constraint that 2 terms should not be equal to each other
--   (a != b), for use in the body of a relation.
(.!=) :: Term ctx ty -> Term ctx ty -> Body ctx ()
(.!=) = addConstraint IsNotEqual
infix 1 .!=

addConstraint :: Op2 -> Term ctx ty -> Term ctx ty -> Body ctx ()
addConstraint op e1 e2 =
  let expr = BinOp' op (toTerm e1) (toTerm e2)
   in tell [Constrain' expr]

-- | Binary AND operator.
band :: (Num ty, Integral ty) => Term ctx ty -> Term ctx ty -> Term ctx ty
band = BinOp BinaryAnd

-- | Binary OR operator.
bor :: (Num ty, Integral ty) => Term ctx ty -> Term ctx ty -> Term ctx ty
bor = BinOp BinaryOr

-- | Binary XOR operator.
bxor :: (Num ty, Integral ty) => Term ctx ty -> Term ctx ty -> Term ctx ty
bxor = BinOp BinaryXor

-- | Logical AND operator.
land :: (Num ty, Integral ty) => Term ctx ty -> Term ctx ty -> Term ctx ty
land = BinOp LogicalAnd

-- | Logical OR operator.
lor :: (Num ty, Integral ty) => Term ctx ty -> Term ctx ty -> Term ctx ty
lor = BinOp LogicalOr

-- | "max" function.
max' :: Num ty => Term ctx ty -> Term ctx ty -> Term ctx ty
max' = func2 Max

-- | "min" function.
min' :: Num ty => Term ctx ty -> Term ctx ty -> Term ctx ty
min' = func2 Min

-- | "cat" function (string concatenation).
cat :: ToString ty => Term ctx ty -> Term ctx ty -> Term ctx ty
cat = func2 Cat

-- | "contains" predicate, checks if 2nd string contains the first.
contains :: ToString ty => Term ctx ty -> Term ctx ty -> Body ctx ()
contains a b =
  let expr = toTerm $ func2 Contains a b
   in tell [Constrain' expr]

-- | "match" predicate, checks if a wildcard string matches a given string.
match :: ToString ty => Term ctx ty -> Term ctx ty -> Body ctx ()
match p s =
  let expr = toTerm $ func2 Match p s
  in tell [Constrain' expr]

-- | "ord" function.
ord :: ToString ty => Term ctx ty -> Term ctx Int32
ord = func1 Ord

-- | "strlen" function.
strlen :: ToString ty => Term ctx ty -> Term ctx Int32
strlen = func1 StrLen

-- | "substr" function.
substr :: ToString ty => Term ctx ty -> Term ctx Int32 -> Term ctx Int32 -> Term ctx ty
substr a b c = Func Substr $ toTerm a :| [toTerm b, toTerm c]

-- | "to_number" function.
to_number :: ToString ty => Term ctx ty -> Term ctx Int32
to_number = func1 ToNumber

-- | "to_string" function.
to_string :: ToString ty => Term ctx Int32 -> Term ctx ty
to_string = func1 ToString

func1 :: FuncName -> Term ctx ty -> Term ctx ty2
func1 name a = Func name $ toTerm a :| []

func2 :: FuncName -> Term ctx ty -> Term ctx ty -> Term ctx ty2
func2 name a b = Func name $ toTerm a :| [toTerm b]

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

data SimpleMetadata = SimpleMetadata StructureOption InlineOption

data StructureOption
  = AutomaticLayout
  | BTreeLayout
  | BrieLayout
  | EqRelLayout

data InlineOption
  = DoInline
  | DoNotInline

data AST
  = Declare' VarName Direction [FieldData] SimpleMetadata
  | Rule' Name (NonEmpty SimpleTerm) AST
  | Atom' Name (NonEmpty SimpleTerm)
  | And' [AST]
  | Or' [AST]
  | Not' AST
  | Constrain' SimpleTerm

data DL
  = Statements [DL]
  | Declare VarName Direction [FieldData] SimpleMetadata
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

-- | A type synonym for a tuple consisting of Datalog 'Term's.
--   Only tuples containing up to 10 elements are currently supported.
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
  Func name ts -> Func' name ts


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

-- | A helper type family for computing the list of types used in a data type.
--   (The type family assumes a data type with a single data constructor.)
type family Structure a :: [Type] where
  Structure a = Collect (Rep a)

type family Collect (a :: Type -> Type) where
  Collect (a :*: b) = Collect a ++ Collect b
  Collect (M1 _ _ a) = Collect a
  Collect (K1 _ ty) = '[ty]

type family a ++ b = c where
  '[] ++ b = b
  a ++ '[] = a
  (a ': b) ++ c = a ': (b ++ c)

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

