{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, UndecidableInstances #-}

-- | A helper module for generating more user friendly type errors in the form
--   of custom constraints.
--   This is an internal module, not meant to be used directly.
module Language.Souffle.Internal.Constraints
  ( SimpleProduct
  ) where

import Type.Errors.Pretty
import GHC.Generics
import Data.Kind
import Data.Int
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


-- | A helper type family used for generating a more user-friendly type error
--   for incompatible types when generically deriving marshalling code for
--   the 'Language.Souffle.Marshal.Marshal' typeclass.
--
--   The __a__ type parameter is the original type, used when displaying the type error.
--   The __f__ type parameter should be equal to 'Rep a', used for analyzing the
--   structure of the data type.
--
--   A type error is returned if the passed in type is not a simple product type
--   consisting of only simple types like Int32, String and Text.
type family SimpleProduct (a :: Type) (f :: Type -> Type) :: Constraint where
  SimpleProduct a f = (ProductLike a f, OnlySimpleFields a f)

type family ProductLike (t :: Type) (f :: Type -> Type) :: Constraint where
  ProductLike t (_ :*: b) = ProductLike t b
  ProductLike t (M1 _ _ a) = ProductLike t a
  ProductLike _ (K1 _ _) = ()
  ProductLike t (_ :+: _) =
    TypeError ( "Error while deriving marshalling code for type " <> t <> ":"
              % "Cannot derive sum type, only product types are supported.")
  ProductLike t U1 =
    TypeError ( "Error while deriving marshalling code for type " <> t <> ":"
              % "Cannot automatically derive code for 0 argument constructor.")
  ProductLike t V1 =
    TypeError ( "Error while deriving marshalling code for type " <> t <> ":"
              % "Cannot derive void type.")

type family OnlySimpleFields (t :: Type) (f :: Type -> Type) :: Constraint where
  OnlySimpleFields t (a :*: b) = (OnlySimpleFields t a, OnlySimpleFields t b)
  OnlySimpleFields t (a :+: b) = (OnlySimpleFields t a, OnlySimpleFields t b)
  OnlySimpleFields t (M1 _ _ a) = OnlySimpleFields t a
  OnlySimpleFields _ U1 = ()
  OnlySimpleFields _ V1 = ()
  OnlySimpleFields t k = OnlySimpleField t k

type family OnlySimpleField (a :: Type) (f :: Type -> Type) :: Constraint where
  OnlySimpleField t (M1 _ _ a) = OnlySimpleField t a
  OnlySimpleField t (K1 _ a) = DirectlyMarshallable t a

type family DirectlyMarshallable (a :: Type) (b :: Type) :: Constraint where
  DirectlyMarshallable _ T.Text = ()
  DirectlyMarshallable _ TL.Text = ()
  DirectlyMarshallable _ Int32 = ()
  DirectlyMarshallable _ Word32 = ()
  DirectlyMarshallable _ Float = ()
  DirectlyMarshallable _ String = ()
  DirectlyMarshallable t a =
    TypeError ( "Error while generating marshalling code for " <> t <> ":"
              % "Can only marshal values of Int32, Word32, Float, String and Text directly"
             <> ", but found " <> a <> " type instead.")

