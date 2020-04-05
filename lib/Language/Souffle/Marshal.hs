
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DerivingVia, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE UndecidableInstances, DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

-- | This module exposes a uniform interface to marshal values
--   to and from Souffle Datalog. This is done via the 'Marshal' typeclass
--   and 'MarshalT' monad transformer.
--   Also, a mechanism is exposed for generically deriving marshalling
--   and unmarshalling code for simple product types.
module Language.Souffle.Marshal
  ( MarshalT
  , runMarshalT
  , Marshal(..)
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS
import GHC.Generics
import Foreign.Ptr
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Souffle.Internal as Internal
import qualified Language.Souffle.Internal.Constraints as C


type Tuple = Ptr Internal.Tuple

-- | A monad transformer, used solely for marshalling and unmarshalling
--   between Haskell and Souffle Datalog.
newtype MarshalT m a = MarshalT (ReaderT Tuple m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Tuple, MonadWriter w
           , MonadState s, MonadRWS Tuple w s, MonadError e )
  via ( ReaderT Tuple m )
  deriving MonadTrans via (ReaderT Tuple)

-- | Execute the monad transformer and return the result.
--   The tuple that is passed in will be used to marshal the data back and forth.
runMarshalT :: MarshalT m a -> Tuple -> m a
runMarshalT (MarshalT m) = runReaderT m
{-# INLINABLE runMarshalT #-}


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
  push :: MonadIO m => a -> MarshalT m ()
  -- | Unmarshals a value from the datalog side.
  pop :: MonadIO m => MarshalT m a

  default push :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a), MonadIO m)
               => a -> MarshalT m ()
  default pop :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a), MonadIO m)
              => MarshalT m a
  push a = gpush (from a)
  {-# INLINABLE push #-}
  pop = to <$> gpop
  {-# INLINABLE pop #-}

instance Marshal Int32 where
  push int = do
    tuple <- ask
    liftIO $ Internal.tuplePushInt tuple int
  {-# INLINABLE push #-}
  pop = do
    tuple <- ask
    liftIO $ Internal.tuplePopInt tuple
  {-# INLINABLE pop #-}

instance Marshal String where
  push str = do
    tuple <- ask
    liftIO $ Internal.tuplePushString tuple str
  {-# INLINABLE push #-}
  pop = do
    tuple <- ask
    liftIO $ Internal.tuplePopString tuple
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
  gpush :: MonadIO m => f a -> MarshalT m ()
  gpop :: MonadIO m => MarshalT m (f a)

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

