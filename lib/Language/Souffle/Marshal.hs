{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, TypeOperators #-}

-- | This module exposes a uniform interface to marshal values
--   to and from Souffle Datalog. This is done via the 'Marshal' typeclass
--   and 'MonadMarshal' monad.
--   Also, a mechanism is exposed for generically deriving marshalling
--   and unmarshalling code for simple product types.
module Language.Souffle.Marshal
  ( Marshal(..)
  , MonadPush(..)
  , MonadPop(..)
  ) where

import GHC.Generics
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Souffle.Internal.Constraints as C

-- TODO docs
class Monad m => MonadPush m where
  pushInt :: Int32 -> m ()
  pushString :: String -> m ()

-- TODO docs
class Monad m => MonadPop m where
  popInt :: m Int32
  popString :: m String


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
  push :: MonadPush m => a -> m ()
  -- | Unmarshals a value from the datalog side.
  pop :: MonadPop m => m a

  default push
    :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a), MonadPush m)
    => a -> m ()
  default pop
    :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a), MonadPop m)
    => m a
  push a = gpush (from a)
  {-# INLINABLE push #-}
  pop = to <$> gpop
  {-# INLINABLE pop #-}

instance Marshal Int32 where
  push = pushInt
  {-# INLINABLE push #-}
  pop = popInt
  {-# INLINABLE pop #-}

instance Marshal String where
  push = pushString
  {-# INLINABLE push #-}
  pop = popString
  {-# INLINABLE pop #-}

instance Marshal T.Text where
  push = push . T.unpack
  {-# INLINABLE push #-}
  pop = T.pack <$> pop
  {-# INLINABLE pop #-}

instance Marshal TL.Text where
  push = push . TL.unpack
  {-# INLINABLE push #-}
  pop = TL.pack <$> pop
  {-# INLINABLE pop #-}

class GMarshal f where
  gpush :: MonadPush m => f a -> m ()
  gpop  :: MonadPop m => m (f a)

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
