
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DerivingVia, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE UndecidableInstances, DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Language.Souffle.Marshal
  ( MarshalT(..)
  , Marshal(..)
  , runMarshalT
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS
import GHC.Generics
import Foreign.Ptr
import Data.Int
import qualified Language.Souffle.Internal as Internal
import qualified Language.Souffle.Internal.Constraints as C


type Tuple = Ptr Internal.Tuple

newtype MarshalT m a = MarshalT (ReaderT Tuple m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Tuple, MonadWriter w
           , MonadState s, MonadRWS Tuple w s, MonadError e )
  via ( ReaderT Tuple m )
  deriving ( MonadTrans ) via (ReaderT Tuple )

runMarshalT :: MarshalT m a -> Tuple -> m a
runMarshalT (MarshalT m) = runReaderT m


class Marshal a where
  push :: MonadIO m => a -> MarshalT m ()
  pop :: MonadIO m => MarshalT m a

  default push :: (Generic a, C.SimpleProduct (Rep a), GMarshal (Rep a), MonadIO m)
               => a -> MarshalT m ()
  default pop :: (Generic a, C.SimpleProduct (Rep a), GMarshal (Rep a), MonadIO m)
              => MarshalT m a
  push a = gpush (from a)
  pop = to <$> gpop

instance Marshal Int32 where
  push int = do
    tuple <- ask
    liftIO $ Internal.tuplePushInt tuple int
  pop = do
    tuple <- ask
    liftIO $ Internal.tuplePopInt tuple

instance Marshal String where
  push str = do
    tuple <- ask
    liftIO $ Internal.tuplePushString tuple str
  pop = do
    tuple <- ask
    liftIO $ Internal.tuplePopString tuple


class GMarshal f where
  gpush :: MonadIO m => f a -> MarshalT m ()
  gpop :: MonadIO m => MarshalT m (f a)

instance Marshal a => GMarshal (K1 i a) where
  gpush (K1 x) = push x
  gpop = K1 <$> pop

instance (GMarshal f, GMarshal g) => GMarshal (f :*: g) where
  gpush (a :*: b) = do
    gpush a
    gpush b
  gpop = (:*:) <$> gpop <*> gpop

instance GMarshal a => GMarshal (M1 i c a) where
  gpush (M1 x) = gpush x
  gpop = M1 <$> gpop

