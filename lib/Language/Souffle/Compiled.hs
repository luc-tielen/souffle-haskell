{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DerivingVia, InstanceSigs, BangPatterns #-}
{-# LANGUAGE DataKinds, FlexibleContexts #-}

-- | This module provides an implementation for the typeclasses defined in
--   "Language.Souffle.Class".
--   It makes use of the low level Souffle C++ API to offer a much more
--   performant alternative implementation to the implementation in
--   "Language.Souffle.Interpreted".
--
--   This module is mainly intended to be used after the prototyping phase is
--   over since the iteration cycle is slower due to the additional
--   C++ compilation times.
module Language.Souffle.Compiled
  ( Program(..)
  , Fact(..)
  , Marshal(..)
  , Handle
  , SouffleM
  , runSouffle
  ) where

import Prelude hiding ( init )

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Data.Foldable ( traverse_ )
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Foreign.ForeignPtr
import Foreign.Ptr
import Language.Souffle.Class
import qualified Language.Souffle.Internal as Internal
import Language.Souffle.Marshal


-- | A datatype representing a handle to a datalog program.
--   The type parameter is used for keeping track of which program
--   type the handle belongs to for additional type safety.
newtype Handle prog = Handle (ForeignPtr Internal.Souffle)

-- | A monad for executing Souffle-related actions in.
newtype SouffleM a
  = SouffleM
  { runSouffle :: IO a  -- ^ Returns the underlying IO action.
  } deriving ( Functor, Applicative, Monad, MonadIO ) via IO

type Tuple = Ptr Internal.Tuple

-- | A monad transformer, used solely for marshalling and unmarshalling
--   between Haskell and Souffle Datalog.
newtype MarshalT m a = MarshalT (ReaderT Tuple m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Tuple, MonadWriter w
           , MonadState s, MonadRWS Tuple w s, MonadError e )
  via ( ReaderT Tuple m )
  deriving MonadTrans via (ReaderT Tuple)

runM :: Monad m => MarshalT m a -> Tuple -> m a
runM (MarshalT m) = runReaderT m
{-# INLINABLE runM #-}

-- | Execute the monad transformer and return the result.
--   The tuple that is passed in will be used to marshal the data back and forth.
runPushT :: MonadIO m => MarshalM PushF a -> Tuple -> m a
runPushT = runM . interpret pushAlgM where
  pushAlgM (PushInt int v) = do
    tuple <- ask
    liftIO $ Internal.tuplePushInt tuple int
    pure v
  pushAlgM (PushStr str v) = do
    tuple <- ask
    liftIO $ Internal.tuplePushString tuple str
    pure v
{-# INLINABLE runPushT #-}

-- | Execute the monad transformer and return the result.
--   The tuple that is passed in will be used to marshal the data back and forth.
runPopT :: MonadIO m => MarshalM PopF a -> Tuple -> m a
runPopT = runM . interpret popAlgM where
  popAlgM (PopStr f) = MarshalT $ do
    tuple <- ask
    str   <- liftIO $ Internal.tuplePopString tuple
    pure $ f str
  popAlgM (PopInt f) = MarshalT $ do
    tuple <- ask
    int   <- liftIO $ Internal.tuplePopInt tuple
    pure $ f int
{-# INLINABLE runPopT #-}

class Collect c where
  collect :: Marshal a => Int -> ForeignPtr Internal.RelationIterator -> IO (c a)

instance Collect [] where
  collect factCount = go 0 factCount []
    where
      go idx count acc _ | idx == count = pure acc
      go idx count !acc !it = do
        tuple <- Internal.relationIteratorNext it
        result <- runPopT pop tuple
        go (idx + 1) count (result : acc) it
  {-# INLINABLE collect #-}

instance Collect V.Vector where
  collect factCount iterator = do
    vec <- MV.unsafeNew factCount
    go vec 0 factCount iterator
    where
      go vec idx count _ | idx == count = V.unsafeFreeze vec
      go vec idx count it = do
        tuple <- Internal.relationIteratorNext it
        result <- runPopT pop tuple
        MV.unsafeWrite vec idx result
        go vec (idx + 1) count it
  {-# INLINABLE collect #-}

instance MonadSouffle SouffleM where
  type Handler SouffleM = Handle
  type CollectFacts SouffleM c = Collect c

  init :: forall prog. Program prog
       => prog -> SouffleM (Maybe (Handle prog))
  init _ =
    let progName = programName (Proxy :: Proxy prog)
    in SouffleM $ fmap Handle <$> Internal.init progName
  {-# INLINABLE init #-}

  run (Handle prog) = SouffleM $ Internal.run prog
  {-# INLINABLE run #-}

  setNumThreads (Handle prog) numCores =
    SouffleM $ Internal.setNumThreads prog numCores
  {-# INLINABLE setNumThreads #-}

  getNumThreads (Handle prog) =
    SouffleM $ Internal.getNumThreads prog
  {-# INLINABLE getNumThreads #-}

  addFact :: forall a prog. (Fact a, ContainsFact prog a)
          => Handle prog -> a -> SouffleM ()
  addFact (Handle prog) fact = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    addFact' relation fact
  {-# INLINABLE addFact #-}

  addFacts :: forall t a prog . (Foldable t, Fact a, ContainsFact prog a)
           => Handle prog -> t a -> SouffleM ()
  addFacts (Handle prog) facts = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    traverse_ (addFact' relation) facts
  {-# INLINABLE addFacts #-}

  getFacts :: forall a c prog. (Fact a, ContainsFact prog a, Collect c)
           => Handle prog -> SouffleM (c a)
  getFacts (Handle prog) = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    factCount <- Internal.countFacts relation
    Internal.getRelationIterator relation >>= collect factCount
  {-# INLINABLE getFacts #-}

  findFact :: forall a prog. (Fact a, ContainsFact prog a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact (Handle prog) a = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    tuple <- Internal.allocTuple relation
    withForeignPtr tuple $ runPushT (push a)
    found <- Internal.containsTuple relation tuple
    pure $ if found then Just a else Nothing
  {-# INLINABLE findFact #-}

addFact' :: Fact a => Ptr Internal.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- Internal.allocTuple relation
  withForeignPtr tuple $ runPushT (push fact)
  Internal.addTuple relation tuple
{-# INLINABLE addFact' #-}


instance MonadSouffleFileIO SouffleM where
  loadFiles (Handle prog) = SouffleM . Internal.loadAll prog
  {-# INLINABLE loadFiles #-}

  writeFiles (Handle prog) = SouffleM $ Internal.printAll prog
  {-# INLINABLE writeFiles #-}

