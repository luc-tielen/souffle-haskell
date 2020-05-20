{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DataKinds, GADTs #-}
{-# LANGUAGE DefaultSignatures, TypeOperators, RankNTypes, MultiParamTypeClasses #-}

-- | This module exposes a uniform interface to marshal values
--   to and from Souffle Datalog. This is done via the 'Marshal' typeclass
--   and 'MonadMarshal' monad.
--   Also, a mechanism is exposed for generically deriving marshalling
--   and unmarshalling code for simple product types.
module Language.Souffle.Marshal
  ( Marshal(..)
  , MonadMarshal(..)
  , Direction(..) -- TODO remove
  ) where

import GHC.Generics
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Souffle.Internal.Constraints as C

data Direction = Push | Pop

-- TODO better name (emphasis on action?), make API simpler, docs
class Monad (m d) => MonadMarshal d m where
  pushInt :: (d ~ 'Push) => Int32 -> m d ()
  pushString :: (d ~ 'Push) => String -> m d ()

  popInt :: (d ~ 'Pop) => m d Int32
  popString :: (d ~ 'Pop) => m d String


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
  push :: MonadMarshal 'Push m => a -> m 'Push ()
  -- | Unmarshals a value from the datalog side.
  pop :: MonadMarshal 'Pop m => m 'Pop a

  default push :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a), MonadMarshal 'Push m)
               => a -> m 'Push ()
  default pop :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a), MonadMarshal 'Pop m)
              => m 'Pop a
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
  gpush :: MonadMarshal 'Push m => f a -> m 'Push ()
  gpop  :: MonadMarshal 'Pop m => m 'Pop (f a)

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
