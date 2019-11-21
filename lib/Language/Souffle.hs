
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE DerivingVia, TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, ConstraintKinds #-}

module Language.Souffle
  ( Program(..)
  , Souffle  -- TODO can be removed?
  , Fact(..)
  , Marshal(..)
  , init
  , run
  , loadFiles
  , writeFiles
  , addFact
  , addFacts
  , getFacts
  ) where

import Prelude hiding ( init )
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS
import Data.Foldable ( traverse_ )
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import Data.Int
import qualified Language.Souffle.Internal as Internal

-- TODO import Language.Souffle.Monad here, and dont use IO directly


newtype Souffle prog = Souffle (ForeignPtr Internal.Souffle)

type Tuple = Ptr Internal.Tuple

newtype MarshalT m a = MarshalT (ReaderT Tuple m a)
  deriving ( Functor, Applicative, Monad
           , MonadIO, MonadReader Tuple, MonadWriter w
           , MonadState s, MonadRWS Tuple w s, MonadError e )
  via ( ReaderT Tuple m )
  deriving ( MonadTrans ) via (ReaderT Tuple )

runMarshalT :: MarshalT m a -> Tuple -> m a
runMarshalT (MarshalT m) = runReaderT m


class Program a where
  type ProgramName a = (s :: Symbol) | s -> a
  type ProgramFacts a :: [Type]

type family ContainsFact prog fact :: Constraint where
  ContainsFact prog fact =
    CheckContains prog (ProgramFacts prog) fact

type family CheckContains prog facts fact :: Constraint where
  CheckContains prog '[] fact =
    (TypeError ('Text "Program of type '" ':<>: 'ShowType prog
          ':<>: 'Text "' does not contain fact: '"
          ':<>: 'ShowType fact ':<>: 'Text "'"))
  CheckContains _ (a ': _) a = ()
  CheckContains prog (_ ': as) b = CheckContains prog as b


class Marshal a => Fact a where
  type FactName a = (s :: Symbol) | s -> a

type KnownFactName a s = (Fact a, KnownSymbol s, FactName a ~ s)

factName :: forall s a. (KnownSymbol s, FactName a ~ s)
         => Proxy a -> String
factName _ = symbolVal (Proxy :: Proxy (FactName a))

class Marshal a where
  push :: MonadIO m => a -> MarshalT m ()
  pop :: MonadIO m => MarshalT m a

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

init :: forall prog s. (KnownSymbol s, ProgramName prog ~ s)
     => prog -> IO (Maybe (Souffle prog))
init _ =
  let progName = programName (Proxy :: Proxy prog)
   in fmap Souffle <$> Internal.init progName

programName :: forall prog s. (KnownSymbol s, ProgramName prog ~ s)
            => Proxy prog -> String
programName _ = symbolVal (Proxy :: Proxy (ProgramName prog))

run :: Souffle prog -> IO ()
run (Souffle prog) = Internal.run prog

loadFiles :: Souffle prog -> String -> IO ()
loadFiles (Souffle prog) = Internal.loadAll prog

writeFiles :: Souffle prog -> IO ()
writeFiles (Souffle prog) = Internal.printAll prog

getFacts ::
  forall a s prog.
  (ContainsFact prog a, KnownFactName a s) =>
  Souffle prog -> IO [a]
getFacts (Souffle prog) = do
  let relationName = factName (Proxy :: Proxy a)
  relation <- Internal.getRelation prog relationName
  Internal.getRelationIterator relation >>= go []
  where
    go acc it = do
      hasNext <- Internal.relationIteratorHasNext it
      if hasNext
        then do
          tuple <- Internal.relationIteratorNext it
          result <- runMarshalT pop tuple
          go (result : acc) it
        else pure acc

addFact ::
  forall a s prog.
  (ContainsFact prog a, KnownFactName a s) =>
  Souffle prog -> a -> IO ()
addFact (Souffle prog) fact = do
  let relationName = factName (Proxy :: Proxy a)
  relation <- Internal.getRelation prog relationName
  addFact' relation fact

addFacts ::
  forall a s prog.
  (ContainsFact prog a, KnownFactName a s) =>
  Souffle prog -> [a] -> IO ()
addFacts (Souffle prog) facts = do
  let relationName = factName (Proxy :: Proxy a)
  relation <- Internal.getRelation prog relationName
  traverse_ (addFact' relation) facts

addFact' :: Fact a => Ptr Internal.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- Internal.allocTuple relation
  withForeignPtr tuple $ runMarshalT (push fact)
  Internal.addTuple relation tuple

