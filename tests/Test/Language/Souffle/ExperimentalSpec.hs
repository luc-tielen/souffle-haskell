
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, TypeApplications, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}

module Test.Language.Souffle.ExperimentalSpec
  ( module Test.Language.Souffle.ExperimentalSpec
  ) where

import Prelude hiding ((^))
import Test.Hspec
import GHC.Generics
import Data.Int
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.Souffle.Experimental
import Language.Souffle.Class
import NeatInterpolation


data Point = Point { x :: Int32, y :: Int32 }
  deriving (Generic, Marshal)

data IntFact = IntFact Int32
  deriving (Generic, Marshal)

data UnsignedFact = UnsignedFact Word32
  deriving (Generic, Marshal)

data FloatFact = FloatFact Float
  deriving (Generic, Marshal)

data TextFact = TextFact T.Text TL.Text
  deriving (Generic, Marshal)

data Triple = Triple String Int32 String
  deriving (Generic, Marshal)

data Vertex = Vertex String
  deriving (Generic, Marshal)

data Edge = Edge String String
  deriving (Generic, Marshal)

data Reachable = Reachable String String
  deriving (Generic, Marshal)

instance Fact Point where
  type FactDirection Point = 'Output
  factName = const "point"
instance Fact IntFact where
  type FactDirection IntFact = 'Output
  factName = const "intfact"
instance Fact FloatFact where
  type FactDirection FloatFact = 'Output
  factName = const "floatfact"
instance Fact UnsignedFact where
  type FactDirection UnsignedFact = 'Output
  factName = const "unsignedfact"
instance Fact TextFact where
  type FactDirection TextFact = 'Output
  factName = const "textfact"
instance Fact Triple where
  type FactDirection Triple = 'Output
  factName = const "triple"
instance Fact Vertex where
  type FactDirection Vertex = 'Output
  factName = const "vertex"
instance Fact Edge where
  type FactDirection Edge = 'Output
  factName = const "edge"
instance Fact Reachable where
  type FactDirection Reachable = 'Output
  factName = const "reachable"


spec :: Spec
spec = fdescribe "Souffle DSL" $ parallel $ do
  describe "code generation" $ parallel $ do
    let prog ==> txt = render (runDSL prog) `shouldBe` (txt <> "\n")

    it "can render an empty program" $ do
      render (runDSL $ pure ()) `shouldBe` ""

    it "can render a program with an input type definition" $ do
      let prog = do
            Predicate _ <- typeDef @Edge Input
            pure ()
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        |]

    it "can render a program with an output type definition" $ do
      let prog = do
            Predicate _ <- typeDef @Edge Output
            pure ()
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .output edge
        |]

    it "can render a program with type declared both as in- and output" $ do
      let prog = do
            Predicate _ <- typeDef @Edge InputOutput
            pure ()
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        .input edge
        .output edge
        |]

    it "can render a program with type declared and only used internally" $ do
      let prog = do
            Predicate _ <- typeDef @Edge Internal
            pure ()
      prog ==> [text|
        .decl edge(t1: symbol, t2: symbol)
        |]

    it "renders type declaration based on type info" $ do
      let prog = do
            Predicate _ <- typeDef @IntFact Input
            Predicate _ <- typeDef @UnsignedFact Input
            Predicate _ <- typeDef @FloatFact Input
            Predicate _ <- typeDef @Triple Output
            pure ()
      prog ==> [text|
        .decl intfact(t1: number)
        .input intfact
        .decl unsignedfact(t1: unsigned)
        .input unsignedfact
        .decl floatfact(t1: float)
        .input floatfact
        .decl triple(t1: symbol, t2: number, t3: symbol)
        .output triple
        |]

    it "uses record accessors as attribute names in type declaration if provided" $ do
      let prog = do
            Predicate _ <- typeDef @Point Input
            pure ()
      prog ==> [text|
        .decl point(x: number, y: number)
        .input point
        |]

    it "can render facts" $ do
      let prog = do
            Predicate edge <- typeDef @Edge Input
            Predicate triple <- typeDef @Triple Input
            Predicate txt <- typeDef @TextFact Input
            Predicate unsigned <- typeDef @UnsignedFact Input
            Predicate float <- typeDef @FloatFact Input
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
        .input triple
        .decl textfact(t1: symbol, t2: symbol)
        .input textfact
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
            Predicate edge <- typeDef @Edge Input
            Predicate reachable <- typeDef @Reachable Output
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
            Predicate edge <- typeDef @Edge Input
            Predicate reachable <- typeDef @Reachable Output
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
            Predicate edge <- typeDef @Edge Input
            Predicate vertex <- typeDef @Vertex Output
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
            Predicate edge <- typeDef @Edge Input
            Predicate reachable <- typeDef @Reachable Output
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
            Predicate edge <- typeDef @Edge Input
            Predicate reachable <- typeDef @Reachable Output
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
            Predicate edge <- typeDef @Edge Input
            Predicate reachable <- typeDef @Reachable Output
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
            Predicate edge <- typeDef @Edge Input
            Predicate reachable <- typeDef @Reachable Output
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
            Predicate reachable <- typeDef @Reachable Output
            a <- var "a"
            b <- var "b"
            reachable(a, b) |- do
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
            Predicate reachable <- typeDef @Reachable Output
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
      let transitive :: forall p1 p2 t. Structure p1 ~ Structure p2
                     => Structure p1 ~ '[t, t]
                     => Predicate p1 -> Predicate p2 -> DSL 'Definition ()
          transitive (Predicate p1) (Predicate p2) = do
            a <- var "a"
            b <- var "b"
            c <- var "c"
            p1(a, b) |- p2(a, b)
            p1(a, b) |- do
              p2(a, c)
              p1(c, b)
          prog = do
            edge <- typeDef @Edge Input
            reachable <- typeDef @Reachable Output
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
            Predicate edge <- typeDef @Edge Input
            Predicate triple <- typeDef @Triple Output
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
        .output triple
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
            Predicate edge <- typeDef @Edge Input
            Predicate reachable <- typeDef @Reachable Output
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate float <- typeDef @FloatFact Input
                float(13.37 / 0.01)
          prog ==> [text|
            .decl floatfact(t1: float)
            .input floatfact
            floatfact(13.37 / 0.01).
            |]

        it "supports ^" $ do
          let prog = do
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
                int(10 ^ 32)
                int(10 ^ 80 ^ 10)
                unsigned(10 ^ 32)
                float(42.42 ^ 2)
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                int(10 % 32)
                int(10 % 80 % 10)
                unsigned(10 % 32)
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
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

      describe "comparisons and equality operators" $ parallel $ do
        -- NOTE: the following generated programs are not correct
        -- since vars are not grounded (but is done to keep tests succinct)
        it "supports <" $ do
          let prog = do
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
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
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
                a <- var "a"
                b <- var "b"
                c <- var "c"
                int(a) |- a .= 10
                unsigned(b) |- b .= 10
                float(c) |- c .= 10.1
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(a) :-
              a = 10.
            unsignedfact(b) :-
              b = 10.
            floatfact(c) :-
              c = 10.1.
            |]

        it "supports !=" $ do
          let prog = do
                Predicate int <- typeDef @IntFact Input
                Predicate unsigned <- typeDef @UnsignedFact Input
                Predicate float <- typeDef @FloatFact Input
                a <- var "a"
                b <- var "b"
                c <- var "c"
                int(a) |- a .!= 10
                unsigned(b) |- b .!= 10
                float(c) |- c .!= 10.1
          prog ==> [text|
            .decl intfact(t1: number)
            .input intfact
            .decl unsignedfact(t1: unsigned)
            .input unsignedfact
            .decl floatfact(t1: float)
            .input floatfact
            intfact(a) :-
              a != 10.
            unsignedfact(b) :-
              b != 10.
            floatfact(c) :-
              c != 10.1.
            |]

    describe "functors" $ parallel $ do
      it "supports max" $ do
        let prog = do
              Predicate int <- typeDef @IntFact Input
              Predicate unsigned <- typeDef @UnsignedFact Input
              Predicate float <- typeDef @FloatFact Input
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
              Predicate int <- typeDef @IntFact Input
              Predicate unsigned <- typeDef @UnsignedFact Input
              Predicate float <- typeDef @FloatFact Input
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
