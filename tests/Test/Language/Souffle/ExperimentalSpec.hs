
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeApplications, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module Test.Language.Souffle.ExperimentalSpec
  ( module Test.Language.Souffle.ExperimentalSpec
  ) where

import qualified Test.Language.Souffle.Experimental.Fixtures as F
import Test.Hspec
import GHC.Generics
import Data.Int
import Data.Word
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.IO.Temp
import Language.Souffle.Experimental
import Language.Souffle.Class
import Language.Souffle.Interpreted as I
import Language.Souffle.Compiled as C
import NeatInterpolation


data Point = Point { x :: Int32, y :: Int32 }
  deriving (Generic, Marshal, FactMetadata)

newtype IntFact = IntFact Int32
  deriving (Generic, Marshal, FactMetadata)

newtype UnsignedFact = UnsignedFact Word32
  deriving (Generic, Marshal, FactMetadata)

newtype FloatFact = FloatFact Float
  deriving (Generic, Marshal, FactMetadata)

data TextFact = TextFact T.Text TL.Text
  deriving (Generic, Marshal, FactMetadata)

data Triple = Triple String Int32 String
  deriving (Generic, Marshal, FactMetadata)

newtype Vertex = Vertex String
  deriving (Generic, Marshal, FactMetadata)

data Edge = Edge String String
  deriving (Generic, Marshal, FactMetadata)

data Reachable = Reachable String String
  deriving (Eq, Show, Generic, Marshal, FactMetadata)

newtype BTreeFact = BTreeFact Int32
  deriving (Generic, Marshal)

newtype BrieFact = BrieFact Int32
  deriving (Generic, Marshal)

data EqRelFact = EqRelFact Int32 Int32
  deriving (Generic, Marshal)

data DSLProgram = DSLProgram

instance Program DSLProgram where
  type ProgramFacts DSLProgram =
    [ Point
    , IntFact
    , FloatFact
    , UnsignedFact
    , TextFact
    , BTreeFact
    , BrieFact
    , EqRelFact
    , Triple
    , Vertex
    , Edge
    , Reachable
    ]
  programName = const "dslprogram"

instance Fact Point where
  type FactDirection Point = 'Input
  factName = const "point"
instance Fact IntFact where
  type FactDirection IntFact = 'Input
  factName = const "intfact"
instance Fact FloatFact where
  type FactDirection FloatFact = 'Input
  factName = const "floatfact"
instance Fact UnsignedFact where
  type FactDirection UnsignedFact = 'Input
  factName = const "unsignedfact"
instance Fact TextFact where
  type FactDirection TextFact = 'InputOutput
  factName = const "textfact"
instance Fact Triple where
  type FactDirection Triple = 'Internal
  factName = const "triple"
instance Fact Vertex where
  type FactDirection Vertex = 'Output
  factName = const "vertex"
instance Fact Edge where
  type FactDirection Edge = 'Input
  factName = const "edge"
instance Fact Reachable where
  type FactDirection Reachable = 'Output
  factName = const "reachable"
instance Fact BTreeFact where
  type FactDirection BTreeFact = 'Internal
  factName = const "btreefact"
instance Fact BrieFact where
  type FactDirection BrieFact = 'Internal
  factName = const "briefact"
instance Fact EqRelFact where
  type FactDirection EqRelFact = 'Input
  factName = const "eqrelfact"
instance FactMetadata BTreeFact where
  factOpts = const $ Metadata BTree NoInline
instance FactMetadata BrieFact where
  factOpts = const $ Metadata Brie Inline
instance FactMetadata EqRelFact where
  factOpts = const $ Metadata EqRel NoInline

spec :: Spec
spec = describe "Souffle DSL" $ parallel $ do
  describe "code generation" $ parallel $ do
    let prog ==> txt =
          let rendered = T.strip $ render DSLProgram prog
              expected = T.strip txt
          in rendered `shouldBe` expected

    it "can render an empty program" $
      render DSLProgram (pure ()) `shouldBe` ""

    it "can render a program with an input type definition" $ do
      let prog = do
            Predicate _ <- predicateFor @Edge
            pure ()
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        |]

    it "can render a program with an output type definition" $ do
      let prog = do
            Predicate _ <- predicateFor @Reachable
            pure ()
      prog ==> [text|
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        |]

    it "can render a program with type declared both as in- and output" $ do
      let prog = do
            Predicate _ <- predicateFor @TextFact
            pure ()
      prog ==> [text|
        .decl textfact(t1: symbol, t2: symbol)
        .input textfact
        .output textfact
        |]

    it "can render a program with type declared and only used internally" $ do
      let prog = do
            Predicate _ <- predicateFor @Triple
            pure ()
      prog ==> [text|
        .decl triple(t1: symbol, t2: number, t3: symbol)
        |]

    it "renders type declaration based on type info" $ do
      let prog = do
            Predicate _ <- predicateFor @IntFact
            Predicate _ <- predicateFor @UnsignedFact
            Predicate _ <- predicateFor @FloatFact
            Predicate _ <- predicateFor @Triple
            Predicate _ <- predicateFor @BTreeFact
            Predicate _ <- predicateFor @BrieFact
            Predicate _ <- predicateFor @EqRelFact
            pure ()
      prog ==> [text|
        .decl intfact(t1: number)
        .input intfact
        .decl unsignedfact(t1: unsigned)
        .input unsignedfact
        .decl floatfact(t1: float)
        .input floatfact
        .decl triple(t1: symbol, t2: number, t3: symbol)
        .decl btreefact(t1: number) btree
        .decl briefact(t1: number) brie inline
        .decl eqrelfact(t1: number, t2: number) eqrel
        .input eqrelfact
        |]

    it "uses record accessors as attribute names in type declaration if provided" $ do
      let prog = do
            Predicate _ <- predicateFor @Point
            pure ()
      prog ==> [text|
        .decl point(x: number, y: number)
        .input point
        |]

    it "can render facts" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate triple <- predicateFor @Triple
            Predicate txt <- predicateFor @TextFact
            Predicate unsigned <- predicateFor @UnsignedFact
            Predicate float <- predicateFor @FloatFact
            edge("a", "b")
            triple("cde", 1000, "fgh")
            txt("ijk", "lmn")
            unsigned(42)
            float(42.42)
            float(0.01)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl triple(t1: symbol, t2: number, t3: symbol)
        .decl textfact(t1: symbol, t2: symbol)
        .input textfact
        .output textfact
        .decl unsignedfact(t1: unsigned)
        .input unsignedfact
        .decl floatfact(t1: float)
        .input floatfact
        edge("a", "b").
        triple("cde", 1000, "fgh").
        textfact("ijk", "lmn").
        unsignedfact(42).
        floatfact(42.42).
        floatfact(0.01).
        |]

    it "can render a relation with a single rule" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            reachable(a, b) |- edge(a, b)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, b) :-
          edge(a, b).
        |]

    it "can render a relation with multiple rules" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            reachable(a, b) |- do
              edge(a, a)
              edge(b, b)
              edge(a, b)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, b) :-
          edge(a, a),
          edge(b, b),
          edge(a, b).
        |]

    it "can render a relation containing a wildcard" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate vertex <- predicateFor @Vertex
            a <- var "a"
            vertex(a) |- do
              edge(a, __)
              edge(__, a)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl vertex(t1: symbol)
        .output vertex
        vertex(a) :-
          edge(a, _),
          edge(_, a).
        |]

    it "can render a relation with a logical or in the rule block" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            reachable(a, b) |- do
              let rules1 = do
                    edge(a, a)
                    edge(b, b)
                  rules2 = do
                    edge(a, b)
                    edge(b, a)
              rules1 \/ rules2
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, b) :-
          edge(a, a),
          edge(b, b);
          edge(a, b),
          edge(b, a).
        |]

    it "can render a relation with multiple clauses" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            c <- var "c"
            reachable(a, b) |- edge(a, b)
            reachable(a, b) |- do
              edge(a, c)
              reachable(c, b)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, b) :-
          edge(a, b).
        reachable(a, b) :-
          edge(a, c),
          reachable(c, b).
        |]

    it "can render a mix of and- and or- clauses correctly" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            c <- var "c"
            reachable(a, b) |- do
              edge(a, c) \/ edge(a, b)
              reachable(c, b)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, b) :-
          (edge(a, c);
          edge(a, b)),
          reachable(c, b).
        |]

    it "discards empty alternative blocks" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            c <- var "c"
            reachable(a, b) |- do
              pure () \/ edge(a, c)
              reachable(c, b)
            reachable(a, b) |- do
              edge(a,c) \/ pure ()
              reachable(c, b)
            reachable(a, b) |- do
              pure () \/ do
                edge(a, c)
                reachable(c, b)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, b) :-
          edge(a, c),
          reachable(c, b).
        reachable(a, b) :-
          edge(a, c),
          reachable(c, b).
        reachable(a, b) :-
          edge(a, c),
          reachable(c, b).
        |]

    it "discards rules with empty rule blocks completely" $ do
      let prog = do
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            reachable(a, b) |-
              pure ()
            reachable(a, b) |- do
              pure () \/ pure ()
              pure ()
      prog ==> [text|
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        |]

    it "discards empty negations" $ do
      let prog = do
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            reachable(a, b) |- do
              not' $ pure ()
      prog ==> [text|
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        |]

    it "allows generically describing predicate relations" $ do
      -- NOTE: type signature not required, but it results in more clear type errors
      -- and can serve as documentation.
      let transitive :: forall prog p1 p2 t. Structure p1 ~ Structure p2
                      => Structure p1 ~ '[t, t]
                      => Predicate p1 -> Predicate p2 -> DSL prog 'Definition ()
          transitive (Predicate p1) (Predicate p2) = do
            a <- var "a"
            b <- var "b"
            c <- var "c"
            p1(a, b) |- p2(a, b)
            p1(a, b) |- do
              p2(a, c)
              p1(c, b)
          prog = do
            edge <- predicateFor @Edge
            reachable <- predicateFor @Reachable
            transitive reachable edge
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, b) :-
          edge(a, b).
        reachable(a, b) :-
          edge(a, c),
          reachable(c, b).
        |]

    it "can render logical negation in rule block" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate triple <- predicateFor @Triple
            a <- var "a"
            b <- var "b"
            c <- var "c"
            triple(a, b, c) |- do
              not' $ edge(a,c)
            triple(a, b, c) |- do
              not' $ do
                edge(a,a)
                edge(c,c)
            triple(a, b, c) |- do
              not' $ edge(a,a) \/ edge(c,c)
            triple(a, b, c) |- do
              not' $ not' $ edge(a,a)
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl triple(t1: symbol, t2: number, t3: symbol)
        triple(a, b, c) :-
          !edge(a, c).
        triple(a, b, c) :-
          !(edge(a, a),
          edge(c, c)).
        triple(a, b, c) :-
          !(edge(a, a);
          edge(c, c)).
        triple(a, b, c) :-
          !!edge(a, a).
        |]

    it "generates unique var names to avoid name collisions" $ do
      let prog = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            a' <- var "a"
            reachable(a, a') |- edge(a, a')
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .decl reachable(t1: symbol, t2: symbol)
        .output reachable
        reachable(a, a_1) :-
          edge(a, a_1).
        |]

    describe "operators" $ parallel $ do
      -- TODO: check for number, unsigned, float; check underscore is not allowed
      describe "arithmetic" $ parallel $ do
        it "supports +" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                int(10 + 32)
                int(10 + 80 + 10)
                unsigned(10 + 32)
                float(10.12 + 31.88)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(10 + 32).
            intfact(10 + 80 + 10).
            unsignedfact(10 + 32).
            floatfact(10.12 + 31.88).
            |]

        it "supports *" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                int(10 * 32)
                int(10 * 80 * 10)
                unsigned(10 * 32)
                float(10.12 * 31.88)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(10 * 32).
            intfact(10 * 80 * 10).
            unsignedfact(10 * 32).
            floatfact(10.12 * 31.88).
            |]

        it "supports binary -" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                int(10 - 32)
                int(10 - 80 - 10)
                unsigned(10 - 32)
                float(10.12 - 31.88)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(10 - 32).
            intfact(10 - 80 - 10).
            unsignedfact(10 - 32).
            floatfact(10.12 - 31.88).
            |]

        it "supports unary -" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate float <- predicateFor @FloatFact
                int(-42)
                int(-100)
                float(-13.37)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(-42).
            intfact(-100).
            floatfact(-13.37).
            |]

        it "supports /" $ do
          let prog = do
                Predicate float <- predicateFor @FloatFact
                float(13.37 / 0.01)
          prog ==> [text|
            .decl floatfact(t1: float)
            .input floatfact
            floatfact(13.37 / 0.01).
            |]

        it "supports ^" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                int(10 .^ 32)
                int(10 .^ 80 .^ 10)
                unsigned(10 .^ 32)
                float(42.42 .^ 2)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(10 ^ 32).
            intfact(10 ^ 80 ^ 10).
            unsignedfact(10 ^ 32).
            floatfact(42.42 ^ 2.0).
            |]

        it "supports %" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                int(10 .% 32)
                int(10 .% 80 .% 10)
                unsigned(10 .% 32)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            intfact(10 % 32).
            intfact(10 % 80 % 10).
            unsignedfact(10 % 32).
            |]

      describe "logical operators" $ parallel $ do
        it "supports band" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                int(10 `band` 32)
                int(10 `band` 80 `band` 10)
                unsigned(10 `band` 32)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            intfact(10 band 32).
            intfact(10 band 80 band 10).
            unsignedfact(10 band 32).
            |]

        it "supports bor" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                int(10 `bor` 32)
                int(10 `bor` 80 `bor` 10)
                unsigned(10 `bor` 32)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            intfact(10 bor 32).
            intfact(10 bor 80 bor 10).
            unsignedfact(10 bor 32).
            |]

        it "supports bxor" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                int(10 `bxor` 32)
                int(10 `bxor` 80 `bxor` 10)
                unsigned(10 `bxor` 32)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            intfact(10 bxor 32).
            intfact(10 bxor 80 bxor 10).
            unsignedfact(10 bxor 32).
            |]

        it "supports land" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                int(10 `land` 32)
                int(10 `land` 80 `land` 10)
                unsigned(10 `land` 32)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            intfact(10 land 32).
            intfact(10 land 80 land 10).
            unsignedfact(10 land 32).
            |]

        it "supports lor" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                int(10 `lor` 32)
                int(10 `lor` 80 `lor` 10)
                unsigned(10 `lor` 32)
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            intfact(10 lor 32).
            intfact(10 lor 80 lor 10).
            unsignedfact(10 lor 32).
            |]

      describe "comparisons and equality, inequality" $ parallel $ do
        -- NOTE: the following generated programs are not correct
        -- since vars are not grounded (but is done to keep tests succinct)
        it "supports <" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                a <- var "a"
                b <- var "b"
                c <- var "c"
                int(a) |- a .< 10
                unsigned(b) |- b .< 10
                float(c) |- c .< 10.1
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(a) :-
              a < 10.
            unsignedfact(b) :-
              b < 10.
            floatfact(c) :-
              c < 10.1.
            |]

        it "supports <=" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                a <- var "a"
                b <- var "b"
                c <- var "c"
                int(a) |- a .<= 10
                unsigned(b) |- b .<= 10
                float(c) |- c .<= 10.1
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(a) :-
              a <= 10.
            unsignedfact(b) :-
              b <= 10.
            floatfact(c) :-
              c <= 10.1.
            |]

        it "supports >" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                a <- var "a"
                b <- var "b"
                c <- var "c"
                int(a) |- a .> 10
                unsigned(b) |- b .> 10
                float(c) |- c .> 10.1
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(a) :-
              a > 10.
            unsignedfact(b) :-
              b > 10.
            floatfact(c) :-
              c > 10.1.
            |]

        it "supports >=" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                a <- var "a"
                b <- var "b"
                c <- var "c"
                int(a) |- a .>= 10
                unsigned(b) |- b .>= 10
                float(c) |- c .>= 10.1
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(a) :-
              a >= 10.
            unsignedfact(b) :-
              b >= 10.
            floatfact(c) :-
              c >= 10.1.
            |]

        it "supports =" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                Predicate vertex <- predicateFor @Vertex
                a <- var "a"
                b <- var "b"
                c <- var "c"
                d <- var "d"
                int(a) |- a .= 10
                unsigned(b) |- b .= 10
                float(c) |- c .= 10.1
                vertex(d) |- d .= "abc"
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            .decl vertex(t1: symbol)
            .output vertex
            intfact(a) :-
              a = 10.
            unsignedfact(b) :-
              b = 10.
            floatfact(c) :-
              c = 10.1.
            vertex(d) :-
              d = "abc".
            |]

        it "supports !=" $ do
          let prog = do
                Predicate int <- predicateFor @IntFact
                Predicate unsigned <- predicateFor @UnsignedFact
                Predicate float <- predicateFor @FloatFact
                Predicate vertex <- predicateFor @Vertex
                a <- var "a"
                b <- var "b"
                c <- var "c"
                d <- var "d"
                int(a) |- a .!= 10
                unsigned(b) |- b .!= 10
                float(c) |- c .!= 10.1
                vertex(d) |- d .!= "abc"
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            .decl vertex(t1: symbol)
            .output vertex
            intfact(a) :-
              a != 10.
            unsignedfact(b) :-
              b != 10.
            floatfact(c) :-
              c != 10.1.
            vertex(d) :-
              d != "abc".
            |]

    describe "functors" $ parallel $ do
      it "supports max" $ do
        let prog = do
              Predicate int <- predicateFor @IntFact
              Predicate unsigned <- predicateFor @UnsignedFact
              Predicate float <- predicateFor @FloatFact
              int(max' 10 32)
              int(max' (max' 10 80) 10)
              unsigned(max' 10 32)
              float(max' 42.42 2)
        prog ==> [text|
          .decl intfact(t1: number)
          .input intfact
          .decl unsignedfact(t1: unsigned)
          .input unsignedfact
          .decl floatfact(t1: float)
          .input floatfact
          intfact(max(10, 32)).
          intfact(max(max(10, 80), 10)).
          unsignedfact(max(10, 32)).
          floatfact(max(42.42, 2.0)).
          |]

      it "supports min" $ do
        let prog = do
              Predicate int <- predicateFor @IntFact
              Predicate unsigned <- predicateFor @UnsignedFact
              Predicate float <- predicateFor @FloatFact
              int(min' 10 32)
              int(min' (min' 10 80) 10)
              unsigned(min' 10 32)
              float(min' 42.42 2)
        prog ==> [text|
          .decl intfact(t1: number)
          .input intfact
          .decl unsignedfact(t1: unsigned)
          .input unsignedfact
          .decl floatfact(t1: float)
          .input floatfact
          intfact(min(10, 32)).
          intfact(min(min(10, 80), 10)).
          unsignedfact(min(10, 32)).
          floatfact(min(42.42, 2.0)).
          |]

      it "supports cat" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              vertex(cat "abc" "def")
              vertex(cat "abc" $ cat "def" "ghi")
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          vertex(cat("abc", "def")).
          vertex(cat("abc", cat("def", "ghi"))).
          |]

      it "supports contains" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              Predicate int <- predicateFor @IntFact
              a <- var "a"
              b <- var "b"
              int(0) |- do
                vertex(a)
                vertex(b)
                contains a b
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          .decl intfact(t1: number)
          .input intfact
          intfact(0) :-
            vertex(a),
            vertex(b),
            contains(a, b).
          |]

      it "supports match" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              Predicate int <- predicateFor @IntFact
              a <- var "a"
              int(0) |- do
                vertex(a)
                match "*.a" a
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          .decl intfact(t1: number)
          .input intfact
          intfact(0) :-
            vertex(a),
            match("*.a", a).
          |]

      it "supports ord" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              Predicate int <- predicateFor @IntFact
              a <- var "a"
              int(ord a) |-
                vertex(a)
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          .decl intfact(t1: number)
          .input intfact
          intfact(ord(a)) :-
            vertex(a).
          |]

      it "supports strlen" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              Predicate int <- predicateFor @IntFact
              a <- var "a"
              int(strlen a) |-
                vertex(a)
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          .decl intfact(t1: number)
          .input intfact
          intfact(strlen(a)) :-
            vertex(a).
          |]

      it "supports substr" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              a <- var "a"
              vertex(a) |-
                a .= substr "Hello" 1 3
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          vertex(a) :-
            a = substr("Hello", 1, 3).
          |]


      it "supports to_number" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              Predicate int <- predicateFor @IntFact
              a <- var "a"
              int(to_number a) |-
                vertex(a)
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          .decl intfact(t1: number)
          .input intfact
          intfact(to_number(a)) :-
            vertex(a).
          |]

      it "supports to_string" $ do
        let prog = do
              Predicate vertex <- predicateFor @Vertex
              Predicate int <- predicateFor @IntFact
              a <- var "a"
              vertex(to_string a) |-
                int(a)
        prog ==> [text|
          .decl vertex(t1: symbol)
          .output vertex
          .decl intfact(t1: number)
          .input intfact
          vertex(to_string(a)) :-
            intfact(a).
          |]

  describe "running DSL code directly " $ parallel $ do
    it "can run DSL with default config in interpreted mode" $ do
      let ast = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            c <- var "c"
            reachable(a, b) |- edge(a, b)
            reachable(a, b) |- do
              edge(a, c)
              reachable(c, b)
          action handle = do
            let prog = fromJust handle
            I.addFacts prog [Edge "a" "b", Edge "b" "c"]
            I.run prog
            I.getFacts prog
      rs <- runSouffleInterpreted DSLProgram ast action
      rs `shouldBe` [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "can run DSL with modified config in interpreted mode" $ do
      tmpDir <- getCanonicalTemporaryDirectory
      souffleHsDir <- createTempDirectory tmpDir "souffle-haskell"
      cfg <- I.defaultConfig
      let config = cfg { I.cfgDatalogDir = souffleHsDir }
          ast = do
            Predicate edge <- predicateFor @Edge
            Predicate reachable <- predicateFor @Reachable
            a <- var "a"
            b <- var "b"
            c <- var "c"
            reachable(a, b) |- edge(a, b)
            reachable(a, b) |- do
              edge(a, c)
              reachable(c, b)
          action handle = do
            let prog = fromJust handle
            I.addFacts prog [Edge "a" "b", Edge "b" "c"]
            I.run prog
            I.getFacts prog
      rs <- runSouffleInterpretedWith config DSLProgram ast action
      rs `shouldBe` [Reachable "a" "b", Reachable "a" "c", Reachable "b" "c"]

    it "can run DSL in compiled mode" $ do
      rs <- C.runSouffle F.CompiledProgram $ \handle -> do
        let prog = fromJust handle
        C.addFacts prog [F.Edge "a" "b", F.Edge "b" "c"]
        C.run prog
        C.getFacts prog
      rs `shouldBe` [F.Reachable "b" "c", F.Reachable "a" "c", F.Reachable "a" "b"]

