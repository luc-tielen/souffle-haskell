
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DerivingVia, InstanceSigs, UndecidableInstances, BangPatterns #-}

-- | This module provides the top level API of this library.
--   It makes use of Haskell's powerful typesystem to make certain invalid states
--   impossible to represent. It does this with a small type level DSL for
--   describing properties of the Datalog program (see the 'Program' and 'Fact'
--   typeclasses for more information).
--   This module also provides a MTL-style interface to Souffle related operations
--   so it can be integrated with existing monad transformer stacks.
module Language.Souffle
  ( Program(..)
  , Fact(..)
  , Marshal.Marshal(..)
  , Handle
  , MonadSouffle(..)
  , SouffleM
  , runSouffle
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
import Type.Errors.Pretty
import Data.Proxy
import Data.Kind
import Data.Word
import qualified Language.Souffle.Internal as Internal
import qualified Language.Souffle.Marshal as Marshal


-- | A datatype representing a handle to a datalog program.
--   The type parameter is used for keeping track of which program
--   type the handle belongs to for additional type safety.
newtype Handle prog = Handle (ForeignPtr Internal.Souffle)

-- | A typeclass for describing a datalog program.
--
-- Example usage (assuming the program was generated from path.dl
-- and contains 2 facts: Edge and Reachable):
--
-- @
-- data Path = Path  -- Handle for the datalog program
--
-- instance Program Path where
--   type ProgramFacts Path = '[Edge, Reachable]
--   factName = const "path"
-- @
class Program a where
  -- | A type level list of facts that belong to this program.
  --   This list is used to check that only known facts are added to a program.
  type ProgramFacts a :: [Type]

  -- | Function for obtaining the name of a Datalog program.
  --   This has to be the same as the name of the .dl file (minus the extension).
  --
  -- It uses a 'Proxy' to select the correct instance.
  programName :: Proxy a -> String

-- | A typeclass for data types representing a fact in datalog.
class Marshal.Marshal a => Fact a where
  -- | Function for obtaining the name of a fact
  --   (has to be the same as described in the Datalog program).
  --
  -- It uses a 'Proxy' to select the correct instance.
  --
  -- Example usage:
  --
  -- @
  -- instance Fact Edge where
  --   factName = const "edge"
  -- @
  factName :: Proxy a -> String

type family ContainsFact prog fact :: Constraint where
  ContainsFact prog fact =
    CheckContains prog (ProgramFacts prog) fact

type family CheckContains prog facts fact :: Constraint where
  CheckContains prog '[] fact =
    TypeError ("You tried to perform an action with a fact of type '" <> fact
    <> "' for program '" <> prog <> "'."
    % "The program contains the following facts: " <> ProgramFacts prog <> "."
    % "It does not contain fact: " <> fact <> "."
    % "You can fix this error by adding the type '" <> fact
    <> "' to the ProgramFacts type in the Program instance for " <> prog <> ".")
  CheckContains _ (a ': _) a = ()
  CheckContains prog (_ ': as) b = CheckContains prog as b


-- | A monad for executing Souffle-related actions in.
newtype SouffleM a
  = SouffleM
  { runSouffle :: IO a  -- ^ Returns the underlying IO action.
  } deriving ( Functor, Applicative, Monad, MonadIO ) via IO

-- | A mtl-style typeclass for Souffle-related actions.
class Monad m => MonadSouffle m where
  {- | Initializes a Souffle program.

     The action will return 'Nothing' if it failed to load the Souffle program.
     Otherwise it will return a 'Handle' that can be used in other functions
     in this module.
  -}
  init :: Program prog => prog -> m (Maybe (Handle prog))

  -- | Runs the Souffle program.
  run :: Handle prog -> m ()

  -- | Sets the number of CPU cores this Souffle program should use.
  setNumThreads :: Handle prog -> Word64 -> m ()

  -- | Gets the number of CPU cores this Souffle program should use.
  getNumThreads :: Handle prog -> m Word64

  -- | Load all facts from files in a certain directory.
  loadFiles :: Handle prog -> FilePath -> m ()

  -- | Write out all facts of the program to CSV files
  --   (as defined in the Souffle program).
  writeFiles :: Handle prog -> m ()

  -- | Returns all facts of a program. This function makes use of type inference
  --   to select the type of fact to return.
  getFacts :: (Fact a, ContainsFact prog a)
           => Handle prog -> m [a]

  -- | Searches for a fact in a program.
  --   Returns 'Nothing' if no matching fact was found; otherwise 'Just' the fact.
  --
  --   Conceptually equivalent to @List.find (== fact) <$> getFacts prog@, but this operation
  --   can be implemented much faster.
  findFact :: (Fact a, ContainsFact prog a)
           => Handle prog -> a -> m (Maybe a)

  -- | Adds a fact to the program.
  addFact :: (Fact a, ContainsFact prog a)
          => Handle prog -> a -> m ()

  -- | Adds multiple facts to the program. This function could be implemented
  --   in terms of 'addFact', but this is done as a minor optimization.
  addFacts :: (Foldable t, Fact a, ContainsFact prog a)
           => Handle prog -> t a -> m ()

instance MonadSouffle SouffleM where
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

  addFacts :: forall t a prog. (Foldable t, Fact a, ContainsFact prog a)
           => Handle prog -> t a -> SouffleM ()
  addFacts (Handle prog) facts = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    traverse_ (addFact' relation) facts
  {-# INLINABLE addFacts #-}

  getFacts :: forall a prog. (Fact a, ContainsFact prog a)
           => Handle prog -> SouffleM [a]
  getFacts (Handle prog) = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    Internal.getRelationIterator relation >>= go []
    where
      go !acc !it = do
        hasNext <- Internal.relationIteratorHasNext it
        if hasNext
          then do
            tuple <- Internal.relationIteratorNext it
            result <- Marshal.runMarshalT Marshal.pop tuple
            go (result : acc) it
          else pure acc
  {-# INLINABLE getFacts #-}

  findFact :: forall a prog. (Fact a, ContainsFact prog a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact (Handle prog) a = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    tuple <- Internal.allocTuple relation
    withForeignPtr tuple $ Marshal.runMarshalT (Marshal.push a)
    found <- Internal.containsTuple relation tuple
    pure $ if found then Just a else Nothing
  {-# INLINABLE findFact #-}

addFact' :: Fact a => Ptr Internal.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- Internal.allocTuple relation
  withForeignPtr tuple $ Marshal.runMarshalT (Marshal.push fact)
  Internal.addTuple relation tuple
{-# INLINABLE addFact' #-}


instance MonadSouffle m => MonadSouffle (ReaderT r m) where
  init = lift . init
  {-# INLINABLE init #-}
  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles = lift . writeFiles
  {-# INLINABLE writeFiles #-}
  getFacts = lift . getFacts
  {-# INLINABLE getFacts #-}
  findFact prog = lift . findFact prog
  {-# INLINABLE findFact #-}
  addFact fact = lift . addFact fact
  {-# INLINABLE addFact #-}
  addFacts facts = lift . addFacts facts
  {-# INLINABLE addFacts #-}

instance (Monoid w, MonadSouffle m) => MonadSouffle (WriterT w m) where
  init = lift . init
  {-# INLINABLE init #-}
  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles = lift . writeFiles
  {-# INLINABLE writeFiles #-}
  getFacts = lift . getFacts
  {-# INLINABLE getFacts #-}
  findFact prog = lift . findFact prog
  {-# INLINABLE findFact #-}
  addFact fact = lift . addFact fact
  {-# INLINABLE addFact #-}
  addFacts facts = lift . addFacts facts
  {-# INLINABLE addFacts #-}

instance MonadSouffle m => MonadSouffle (StateT s m) where
  init = lift . init
  {-# INLINABLE init #-}
  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles = lift . writeFiles
  {-# INLINABLE writeFiles #-}
  getFacts = lift . getFacts
  {-# INLINABLE getFacts #-}
  findFact prog = lift . findFact prog
  {-# INLINABLE findFact #-}
  addFact fact = lift . addFact fact
  {-# INLINABLE addFact #-}
  addFacts facts = lift . addFacts facts
  {-# INLINABLE addFacts #-}

instance (MonadSouffle m, Monoid w) => MonadSouffle (RWST r w s m) where
  init = lift . init
  {-# INLINABLE init #-}
  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles = lift . writeFiles
  {-# INLINABLE writeFiles #-}
  getFacts = lift . getFacts
  {-# INLINABLE getFacts #-}
  findFact prog = lift . findFact prog
  {-# INLINABLE findFact #-}
  addFact fact = lift . addFact fact
  {-# INLINABLE addFact #-}
  addFacts facts = lift . addFacts facts
  {-# INLINABLE addFacts #-}

instance MonadSouffle m => MonadSouffle (ExceptT s m) where
  init = lift . init
  {-# INLINABLE init #-}
  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles = lift . writeFiles
  {-# INLINABLE writeFiles #-}
  getFacts = lift . getFacts
  {-# INLINABLE getFacts #-}
  findFact prog = lift . findFact prog
  {-# INLINABLE findFact #-}
  addFact fact = lift . addFact fact
  {-# INLINABLE addFact #-}
  addFacts facts = lift . addFacts facts
  {-# INLINABLE addFacts #-}

