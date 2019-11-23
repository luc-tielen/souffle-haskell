
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DerivingVia, InstanceSigs, UndecidableInstances #-}

module Language.Souffle
  ( Program(..)
  , MonadSouffle(..)
  , SouffleM
  , runSouffle
  , SouffleProgram
  , Fact(..)
  , Marshal.Marshal(..)
  ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import qualified Language.Souffle.Internal as Internal
import qualified Language.Souffle.Marshal as Marshal


newtype SouffleProgram prog = SouffleProgram (ForeignPtr Internal.Souffle)

class Program a where
  type ProgramFacts a :: [Type]
  programName :: Proxy a -> String

class Marshal.Marshal a => Fact a where
  factName :: Proxy a -> String

type family ContainsFact prog fact :: Constraint where
  ContainsFact prog fact =
    CheckContains prog (ProgramFacts prog) fact

type family CheckContains prog facts fact :: Constraint where
  CheckContains prog '[] fact =
    -- TODO list out all facts?
    (TypeError ('Text "Program of type '" ':<>: 'ShowType prog
          ':<>: 'Text "' does not contain fact: '"
          ':<>: 'ShowType fact ':<>: 'Text "'"))
  CheckContains _ (a ': _) a = ()
  CheckContains prog (_ ': as) b = CheckContains prog as b


newtype SouffleM a = SouffleM { runSouffle :: IO a }
  deriving ( Functor, Applicative, Monad, MonadIO ) via IO

class Monad m => MonadSouffle m where
  init :: Program prog => prog -> m (Maybe (SouffleProgram prog))

  run :: SouffleProgram prog -> m ()

  loadFiles :: SouffleProgram prog -> String -> m ()

  writeFiles :: SouffleProgram prog -> m ()

  getFacts :: (Fact a, ContainsFact prog a)
           => SouffleProgram prog -> m [a]

  addFact :: (Fact a, ContainsFact prog a)
          => SouffleProgram prog -> a -> m ()

  addFacts :: (Fact a, ContainsFact prog a)
           => SouffleProgram prog -> [a] -> m ()

instance MonadSouffle SouffleM where
  init :: forall prog. Program prog
       => prog -> SouffleM (Maybe (SouffleProgram prog))
  init _ =
    let progName = programName (Proxy :: Proxy prog)
    in SouffleM $ fmap SouffleProgram <$> Internal.init progName

  run (SouffleProgram prog) = SouffleM $ Internal.run prog

  loadFiles (SouffleProgram prog) = SouffleM . Internal.loadAll prog

  writeFiles (SouffleProgram prog) = SouffleM $ Internal.printAll prog

  addFact :: forall a prog. (Fact a, ContainsFact prog a)
          => SouffleProgram prog -> a -> SouffleM ()
  addFact (SouffleProgram prog) fact = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    addFact' relation fact

  addFacts :: forall a prog. (Fact a, ContainsFact prog a)
           => SouffleProgram prog -> [a] -> SouffleM ()
  addFacts (SouffleProgram prog) facts = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    traverse_ (addFact' relation) facts

  getFacts :: forall a prog. (Fact a, ContainsFact prog a)
           => SouffleProgram prog -> SouffleM [a]
  getFacts (SouffleProgram prog) = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    Internal.getRelationIterator relation >>= go []
    where
      go acc it = do
        hasNext <- Internal.relationIteratorHasNext it
        if hasNext
          then do
            tuple <- Internal.relationIteratorNext it
            result <- Marshal.runMarshalT Marshal.pop tuple
            go (result : acc) it
          else pure acc

addFact' :: Fact a => Ptr Internal.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- Internal.allocTuple relation
  withForeignPtr tuple $ Marshal.runMarshalT (Marshal.push fact)
  Internal.addTuple relation tuple


instance MonadSouffle m => MonadSouffle (ReaderT r m) where
  init = lift . init
  run = lift . run
  loadFiles prog = lift . loadFiles prog
  writeFiles = lift . writeFiles
  getFacts = lift . getFacts
  addFact fact = lift . addFact fact
  addFacts facts = lift . addFacts facts

instance (Monoid w, MonadSouffle m) => MonadSouffle (WriterT w m) where
  init = lift . init
  run = lift . run
  loadFiles prog = lift . loadFiles prog
  writeFiles = lift . writeFiles
  getFacts = lift . getFacts
  addFact fact = lift . addFact fact
  addFacts facts = lift . addFacts facts

instance MonadSouffle m => MonadSouffle (StateT s m) where
  init = lift . init
  run = lift . run
  loadFiles prog = lift . loadFiles prog
  writeFiles = lift . writeFiles
  getFacts = lift . getFacts
  addFact fact = lift . addFact fact
  addFacts facts = lift . addFacts facts

instance (MonadSouffle m, Monoid w) => MonadSouffle (RWST r w s m) where
  init = lift . init
  run = lift . run
  loadFiles prog = lift . loadFiles prog
  writeFiles = lift . writeFiles
  getFacts = lift . getFacts
  addFact fact = lift . addFact fact
  addFacts facts = lift . addFacts facts

instance MonadSouffle m => MonadSouffle (ExceptT s m) where
  init = lift . init
  run = lift . run
  loadFiles prog = lift . loadFiles prog
  writeFiles = lift . writeFiles
  getFacts = lift . getFacts
  addFact fact = lift . addFact fact
  addFacts facts = lift . addFacts facts

