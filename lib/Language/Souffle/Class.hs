{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Souffle.Class
  ( ContainsFact
  , Program(..)
  , Fact(..)
  , MonadSouffle(..)
  ) where

import Prelude hiding ( init )
import Type.Errors.Pretty
import Data.Proxy
import Data.Kind
import Data.Word
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Except
import qualified Language.Souffle.Marshal as Marshal


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

---- | Helper typeclass for collecting facts into a container-like structure.
----   The order of returned facts is unspecified for performance reasons.
----   Only used internally.
--class CollectFacts m c where
--  type CollectFactsIterator m c :: Type
--  collectFacts :: Marshal.Marshal a
--               => Int
--               -> CollectFactsIterator m c
--               -> IO (c a)

-- | A mtl-style typeclass for Souffle-related actions.
class Monad m => MonadSouffle m where
  type Handler m :: Type -> Type
  {- | Initializes a Souffle program.

     The action will return 'Nothing' if it failed to load the Souffle program.
     Otherwise it will return a 'Handle' that can be used in other functions
     in this module.
  -}
  init :: Program prog => prog -> m (Maybe (Handler m prog))

  -- | Runs the Souffle program.
  run :: Handler m prog -> m ()

  -- | Sets the number of CPU cores this Souffle program should use.
  setNumThreads :: Handler m prog -> Word64 -> m ()

  -- | Gets the number of CPU cores this Souffle program should use.
  getNumThreads :: Handler m prog -> m Word64

  -- | Load all facts from files in a certain directory.
  loadFiles :: Handler m prog -> FilePath -> m ()

  -- | Write out all facts of the program to CSV files
  --   (as defined in the Souffle program).
  writeFiles :: Handler m prog -> m ()

  -- | Returns all facts of a program. This function makes use of type inference
  --   to select the type of fact to return.
  getFacts :: (Fact a, ContainsFact prog a)
           => Handler m prog -> m [a]

  -- | Searches for a fact in a program.
  --   Returns 'Nothing' if no matching fact was found; otherwise 'Just' the fact.
  --
  --   Conceptually equivalent to @List.find (== fact) \<$\> getFacts prog@, but this operation
  --   can be implemented much faster.
  findFact :: (Fact a, ContainsFact prog a, Eq a)
           => Handler m prog -> a -> m (Maybe a)

  -- | Adds a fact to the program.
  addFact :: (Fact a, ContainsFact prog a)
          => Handler m prog -> a -> m ()

  -- | Adds multiple facts to the program. This function could be implemented
  --   in terms of 'addFact', but this is done as a minor optimization.
  addFacts :: (Foldable t, Fact a, ContainsFact prog a)
           => Handler m prog -> t a -> m ()

instance MonadSouffle m => MonadSouffle (ReaderT r m) where
  type Handler (ReaderT r m) = Handler m
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
  type Handler (WriterT w m) = Handler m
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
  type Handler (StateT s m) = Handler m
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
  type Handler (RWST r w s m) = Handler m
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
  type Handler (ExceptT s m) = Handler m
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
