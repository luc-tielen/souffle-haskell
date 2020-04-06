{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DerivingVia, InstanceSigs, BangPatterns #-}

-- | This module provides the top level API of this library.
--   It makes use of Haskell's powerful typesystem to make certain invalid states
--   impossible to represent. It does this with a small type level DSL for
--   describing properties of the Datalog program (see the 'Program' and 'Fact'
--   typeclasses for more information).
--   This module also provides a MTL-style interface to Souffle related operations
--   so it can be integrated with existing monad transformer stacks.
module Language.Souffle.Compiled
  ( Program(..)
  , Fact(..)
  , Marshal(..)
  , Handle
  , ContainsFact
  , MonadSouffle(..)
  , SouffleM
  , runSouffle
  ) where

import Prelude hiding ( init )
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Data.Foldable ( traverse_ )
import Data.Proxy
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

-- | Execute the monad transformer and return the result.
--   The tuple that is passed in will be used to marshal the data back and forth.
runMarshalT :: forall m a . (Monad m, MonadIO m) => MarshalM a -> Tuple -> m a
runMarshalT free = calc (interpret marshalAlgM free)
  where
    calc (MarshalT m) = runReaderT m
{-# INLINABLE runMarshalT #-}

marshalAlgM :: (Monad m, MonadIO m) => MarshalF a -> MarshalT m a
marshalAlgM (PopStr f) = MarshalT $ do
  tuple <- ask
  str   <- liftIO $ Internal.tuplePopString tuple
  pure $ f str
marshalAlgM (PopInt f) = MarshalT $ do
  tuple <- ask
  int   <- liftIO $ Internal.tuplePopInt tuple
  pure $ f int
marshalAlgM (PushInt int v) = MarshalT $ do
  tuple <- ask
  liftIO $ Internal.tuplePushInt tuple int
  pure v
marshalAlgM (PushStr str v) = MarshalT $ do
  tuple <- ask
  liftIO $ Internal.tuplePushString tuple str
  pure v
{-# INLINABLE marshalAlgM #-}

collectFacts
  :: Marshal a
  => Int
  -> ForeignPtr Internal.RelationIterator
  -> IO [a]
collectFacts factCount = go 0 factCount []
  where
    go idx count acc _ | idx == count = pure acc
    go idx count !acc !it = do
      tuple <- Internal.relationIteratorNext it
      result <- runMarshalT pop tuple
      go (idx + 1) count (result : acc) it

instance MonadSouffle SouffleM where
  type Handler SouffleM = Handle
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

  loadFiles (Handle prog) = SouffleM . Internal.loadAll prog
  {-# INLINABLE loadFiles #-}

  writeFiles (Handle prog) = SouffleM $ Internal.printAll prog
  {-# INLINABLE writeFiles #-}

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

  getFacts :: forall a prog . (Fact a, ContainsFact prog a)
           => Handle prog -> SouffleM [a]
  getFacts (Handle prog) = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    factCount <- Internal.countFacts relation
    Internal.getRelationIterator relation >>= collectFacts factCount
  {-# INLINABLE getFacts #-}

  findFact :: forall a prog. (Fact a, ContainsFact prog a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact (Handle prog) a = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    tuple <- Internal.allocTuple relation
    withForeignPtr tuple $ runMarshalT (push a)
    found <- Internal.containsTuple relation tuple
    pure $ if found then Just a else Nothing
  {-# INLINABLE findFact #-}

addFact' :: Fact a => Ptr Internal.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- Internal.allocTuple relation
  withForeignPtr tuple $ runMarshalT (push fact)
  Internal.addTuple relation tuple
{-# INLINABLE addFact' #-}
