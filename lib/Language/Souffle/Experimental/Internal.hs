
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeApplications, PolyKinds #-}

module Language.Souffle.Experimental.Internal
  ( TypeInfo(..)
  , Structure
  , NameFor
  , nameFor
  , MapConstraint
  , TupleOf
  , ToAtoms(..)
  ) where

import Language.Souffle.Experimental.Types
import GHC.Generics
import GHC.TypeLits
import Data.Kind
import Data.Int
import Data.Proxy
import Data.Char ( toLower )
import Data.List.NonEmpty ( NonEmpty(..) )


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


nameFor :: forall a. (KnownSymbol (NameFor a)) => Proxy a -> String
nameFor _ = formatName $ symbolVal (Proxy @(NameFor a)) where
  formatName = fmap toLower -- TODO camelcase to snake case?

type family NameFor a where
  NameFor a = GetName (Rep a)

type family GetName (repr :: Type -> Type) :: Symbol where
  GetName (D1 ('MetaData name _ _ _) _) = name

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
  toAtoms :: TypeInfo a ts -> TupleOf ts -> NonEmpty (Atom ctx)

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
  toAtom :: a -> Atom ctx

instance ToAtom Int32 where toAtom = Int
instance ToAtom String where toAtom = Str

