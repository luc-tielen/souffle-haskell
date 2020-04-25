{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DeriveFunctor #-}
{-# LANGUAGE DefaultSignatures, TypeOperators, RankNTypes #-}

-- | This module exposes a uniform interface to marshal values
--   to and from Souffle Datalog. This is done via the 'Marshal' typeclass
--   and 'MarshalM' monad.
--   Also, a mechanism is exposed for generically deriving marshalling
--   and unmarshalling code for simple product types.
module Language.Souffle.Marshal
  ( Marshal(..)
  , PushF(..)
  , PopF(..)
  , MarshalM
  , interpret
  ) where

import GHC.Generics
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Souffle.Internal.Constraints as C


-- | A data type used for deserializing a `Marshal`-able value
--   from Souffle to Haskell, only used internally.
data PopF a
  = PopInt (Int32 -> a)
  | PopStr (String -> a)
  deriving Functor

-- | A data type used for serializing a `Marshal`-able value
--   from Haskell to Souffle, only used internally.
data PushF a
  = PushInt Int32 a
  | PushStr String a
  deriving Functor

-- NOTE: Free is reimplemented here to avoid pulling in quite a few
--       dependencies and since we only need 2 functions

-- | The monad used for serializing and deserializing of values that
--   implement the `Marshal` typeclass.
data MarshalM f a
  = Pure a
  | Free (f (MarshalM f a))
  deriving Functor

instance Functor f => Applicative (MarshalM f) where
  pure = Pure
  {-# INLINABLE pure #-}
  Pure f <*> Pure a = Pure $ f a
  Pure f <*> Free fa = f <$> Free fa
  Free fa <*> m = Free $ fmap (<*> m) fa
  {-# INLINABLE (<*>) #-}

instance Functor f => Monad (MarshalM f) where
  Pure a >>= f = f a
  Free fa >>= f = Free $ fmap (>>= f) fa
  {-# INLINABLE (>>=) #-}

liftF :: Functor f => f a -> MarshalM f a
liftF action = Free $ fmap pure action
{-# INLINABLE liftF #-}

-- | Helper function for interpreting the actual (de-)serialization of values.
--   This allows both the compiled and interpreted variant to handle
--   (de-)serialization in their own way.
interpret :: Monad m => (forall x. f x -> m x) -> MarshalM f a -> m a
interpret f = \case
  Pure a -> pure a
  Free fa -> f fa >>= interpret f
{-# INLINABLE interpret #-}

{- | A typeclass for providing a uniform API to marshal/unmarshal values
     between Haskell and Souffle datalog.

The marshalling is done via a stack-based approach, where elements are
pushed/popped one by one. You need to make sure that the marshalling
of values happens in the correct order or unexpected things might happen
(including crashes). Pushing and popping of fields should happen in the
same order (from left to right, as defined in Datalog).

Generic implementations for 'push' and 'pop' that perform the previously
described behavior are available. This makes it possible to
write very succinct code:

@
data Edge = Edge String String deriving Generic

instance Marshal Edge
@
-}
class Marshal a where
  -- | Marshals a value to the datalog side.
  push :: a -> MarshalM PushF ()
  -- | Unmarshals a value from the datalog side.
  pop :: MarshalM PopF a

  default push :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a))
               => a -> MarshalM PushF ()
  default pop :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a))
              => MarshalM PopF a
  push a = gpush (from a)
  {-# INLINABLE push #-}
  pop = to <$> gpop
  {-# INLINABLE pop #-}

instance Marshal Int32 where
  push int = liftF (PushInt int ())
  {-# INLINABLE push #-}
  pop  = liftF (PopInt id)
  {-# INLINABLE pop #-}

instance Marshal String where
  push str = liftF (PushStr str ())
  {-# INLINABLE push #-}
  pop  = liftF (PopStr id)
  {-# INLINABLE pop #-}

instance Marshal T.Text where
  push = push . T.unpack
  {-# INLINABLE push #-}
  pop  = T.pack <$> pop
  {-# INLINABLE pop #-}

instance Marshal TL.Text where
  push = push . TL.unpack
  {-# INLINABLE push #-}
  pop  = TL.pack <$> pop
  {-# INLINABLE pop #-}

class GMarshal f where
  gpush :: f a -> MarshalM PushF ()
  gpop  :: MarshalM PopF (f a)

instance Marshal a => GMarshal (K1 i a) where
  gpush (K1 x) = push x
  {-# INLINABLE gpush #-}
  gpop = K1 <$> pop
  {-# INLINABLE gpop #-}

instance (GMarshal f, GMarshal g) => GMarshal (f :*: g) where
  gpush (a :*: b) = do
    gpush a
    gpush b
  {-# INLINABLE gpush #-}
  gpop = (:*:) <$> gpop <*> gpop
  {-# INLINABLE gpop #-}

instance GMarshal a => GMarshal (M1 i c a) where
  gpush (M1 x) = gpush x
  {-# INLINABLE gpush #-}
  gpop = M1 <$> gpop
  {-# INLINABLE gpop #-}
