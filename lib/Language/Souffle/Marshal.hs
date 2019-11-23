
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
import GHC.TypeLits
import GHC.Generics
import Foreign.Ptr
import Data.Kind
import Data.Int
import qualified Language.Souffle.Internal as Internal


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

  default push :: (Generic a, SimpleProduct (Rep a), GMarshal (Rep a), MonadIO m)
               => a -> MarshalT m ()
  default pop :: (Generic a, SimpleProduct (Rep a), GMarshal (Rep a), MonadIO m)
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

