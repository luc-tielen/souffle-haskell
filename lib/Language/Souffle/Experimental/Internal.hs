
{-# LANGUAGE TypeFamilies, TypeOperators, DataKinds, UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}

module Language.Souffle.Experimental.Internal
  ( Structure
  , NameFor
  , nameFor
  , MapConstraint
  , TupleOf
  ) where

import GHC.Generics
import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Data.Char ( toLower )


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

