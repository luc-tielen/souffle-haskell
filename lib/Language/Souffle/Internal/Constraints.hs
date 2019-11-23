
{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Souffle.Internal.Constraints
  ( SimpleProduct
  ) where

import GHC.Generics
import GHC.TypeLits
import Data.Kind
import Data.Int


type family SimpleProduct (f :: Type -> Type) :: Constraint where
  SimpleProduct f = (ProductLike f, OnlySimpleFields f)

type family ProductLike (f :: Type -> Type) :: Constraint where
  ProductLike (_ :*: b) = ProductLike b
  ProductLike (M1 _ _ a) = ProductLike a
  ProductLike (K1 _ _) = ()
  ProductLike (_ :+: _) =
    TypeError ('Text "Can't derive sum type from/to datalog fact.")
  ProductLike U1 =
    TypeError ('Text "Can't derive unary type from/to datalog fact automatically.")
  ProductLike V1 =
    TypeError ('Text "Can't derive void type from/to datalog fact.")

type family OnlySimpleFields (f :: Type -> Type) :: Constraint where
  OnlySimpleFields (a :*: b) = (OnlySimpleField a, OnlySimpleFields b)
  OnlySimpleFields (a :+: b) = (OnlySimpleFields a, OnlySimpleFields b)
  OnlySimpleFields (M1 _ _ a) = OnlySimpleFields a
  OnlySimpleFields U1 = ()
  OnlySimpleFields V1 = ()
  OnlySimpleFields k = OnlySimpleField k

type family OnlySimpleField (f :: Type -> Type) :: Constraint where
  OnlySimpleField (M1 _ _ a) = OnlySimpleField a
  OnlySimpleField (K1 _ a) = DirectlyMarshallable a
  OnlySimpleField _ =
    TypeError ('Text "Fact datatype can only contain directly marshallable values")

type family DirectlyMarshallable (a :: Type) :: Constraint where
  DirectlyMarshallable Int32 = ()
  DirectlyMarshallable String = ()
  DirectlyMarshallable a =
    TypeError ('Text "Can only marshal values of Int32 and String directly"
         ':<>: 'Text ", but found type: " ':<>: 'ShowType a ':<>: 'Text ".")

