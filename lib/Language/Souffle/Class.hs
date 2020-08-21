{-# LANGUAGE DataKinds, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE GADTs, TypeFamilies, TypeOperators #-}

-- | This module provides the top level API for Souffle related operations.
--   It makes use of Haskell's powerful typesystem to make certain invalid states
--   impossible to represent. It does this with a small type level DSL for
--   describing properties of the Datalog program (see the 'Program' and 'Fact'
--   typeclasses for more information).
--
--   The Souffle operations are exposed via 2 mtl-style interfaces
--   (see `MonadSouffle` and `MonadSouffleFileIO`) that allows them to be
--   integrated with existing monad transformer stacks.
--
--   This module also contains some helper type families for additional
--   type safety and user-friendly error messages.
module Language.Souffle.Class
  ( Program(..)
  , Fact(..)
  , Marshal.Marshal(..)
  , Direction(..)
  , FactOpts(..)
  , StructureOpt(..)
  , InlineOpt(..)
  , ContainsInputFact
  , ContainsOutputFact
  , ContainsFact
  , MonadSouffle(..)
  , MonadSouffleFileIO(..)
  ) where

import Prelude hiding ( init )

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Proxy
import Data.Kind
import Data.Word
import qualified Language.Souffle.Marshal as Marshal
import Type.Errors.Pretty


-- | A helper type family for checking if a specific Souffle `Program` contains
--   a certain `Fact`. Additionally, it also checks if the fact is marked as
--   either `Input` or `InputOutput`. This constraint will generate a
--   user-friendly type error if these conditions are not met.
type family ContainsInputFact prog fact :: Constraint where
  ContainsInputFact prog fact = (ContainsFact prog fact, IsInput fact (FactDirection fact))

-- | A helper type family for checking if a specific Souffle `Program` contains
--   a certain `Fact`. Additionally, it also checks if the fact is marked as
--   either `Output` or `InputOutput`. This constraint will generate a
--   user-friendly type error if these conditions are not met.
type family ContainsOutputFact prog fact :: Constraint where
  ContainsOutputFact prog fact = (ContainsFact prog fact, IsOutput fact (FactDirection fact))

type family IsInput (fact :: Type) (dir :: Direction) :: Constraint where
  IsInput _ 'Input = ()
  IsInput _ 'InputOutput = ()
  IsInput fact dir = TypeError
    ( "You tried to use an " <> FormatDirection dir <> " fact of type " <> fact <> " as an input."
    % "Possible solution: change the FactDirection of " <> fact
      <> " to either 'Input' or 'InputOutput'."
    )

type family IsOutput (fact :: Type) (dir :: Direction) :: Constraint where
  IsOutput _ 'Output = ()
  IsOutput _ 'InputOutput = ()
  IsOutput fact dir = TypeError
    ( "You tried to use an " <> FormatDirection dir <> " fact of type " <> fact <> " as an output."
    % "Possible solution: change the FactDirection of " <> fact
      <> " to either 'Output' or 'InputOutput'."
    )

type family FormatDirection (dir :: Direction) where
  FormatDirection 'Output = "output"
  FormatDirection 'Input = "input"
  FormatDirection 'Internal = "internal"

-- | A helper type family for checking if a specific Souffle `Program` contains
--   a certain `Fact`. This constraint will generate a user-friendly type error
--   if this is not the case.
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
--   programName = const "path"
-- @
class Program a where
  -- | A type level list of facts that belong to this program.
  --   This list is used to check that only known facts are added to a program.
  type ProgramFacts a :: [Type]

  -- | Function for obtaining the name of a Datalog program.
  --   This has to be the same as the name of the .dl file (minus the extension).
  programName :: a -> String

-- | A typeclass for data types representing a fact in datalog.
--
-- Example usage:
--
-- @
-- instance Fact Edge where
--   type FactDirection Edge = 'Input
--   factName = const "edge"
-- @
class Marshal.Marshal a => Fact a where
  -- | The direction or "mode" a fact can be used in.
  --   This is used to perform compile-time checks that a fact is only used
  --   in valid situations. For more information, see the 'Direction' type.
  type FactDirection a :: Direction

  -- | Function for obtaining the name of a fact
  --   (has to be the same as described in the Datalog program).
  --
  -- It uses a 'Proxy' to select the correct instance.
  factName :: Proxy a -> String

  -- | An optional function for configuring fact metadata.
  --
  --   By default no extra options are configured.
  --   For more information, see the 'FactOpts' type.
  factOpts :: Proxy a -> Maybe (FactOpts (FactDirection a))
  factOpts = const Nothing

-- | A datatype describing which operations a certain fact supports.
--   The direction is from the datalog perspective, so that it
--   aligns with ".decl" statements in Souffle.
data Direction
  = Input
  -- ^ Fact can only be stored in Datalog (using `addFact`/`addFacts`).
  | Output
  -- ^ Fact can only be read from Datalog (using `getFacts`/`findFact`).
  | InputOutput
  -- ^ Fact supports both reading from / writing to Datalog.
  | Internal
  -- ^ Supports neither reading from / writing to Datalog. This is used for
  --   facts that are only visible inside Datalog itself.

-- | A data type that allows for finetuning of fact settings.
--
--   Note: These settings are only taken into account when Datalog code is
--   generated from Haskell (using functions from the
--   'Language.Souffle.Experimental' module). Otherwise the Datalog code
--   itself should contain these fact options.
data FactOpts (d :: Direction)
  = FactOpts StructureOpt (InlineOpt d)

-- | Datatype describing the way a fact is stored inside Datalog.
--   A different choice of storage type can lead to an improvement in
--   performance (potentially).
--
--   Note: This is only applicable for when the Datalog code is generated via
--   the DSL. Otherwise, the Datalog file itself should contain the storage type.
--
--   For more information, see the Souffle
--   <https://souffle-lang.github.io/tuning#datastructure documentation>.
data StructureOpt
  = BTree
  -- ^ The default datastructure for most relations in Souffle. This is storage
  --   type that is used by default.
  | Brie
  -- ^ Can improve performance in some cases, and is more memory efficient for
  --   particularly large relations.
  | EqRel
  -- ^ A high performance datastructure optimised specifically for equivalence
  --   relations. This is only valid for binary facts with 2 fields of the
  --   same type.

-- | Datatype indicating if we should inline a fact or not.
--   Inlining is only possible for internal facts.
--
--   Note: This is only applicable for when the Datalog code is generated via
--   the DSL. Otherwise, the Datalog file itself should contain the storage type.
data InlineOpt d where
  Inline :: InlineOpt 'Internal
  NoInline :: InlineOpt d

-- | A mtl-style typeclass for Souffle-related actions.
class Monad m => MonadSouffle m where
  -- | Represents a handle for interacting with a Souffle program.
  --
  --   The handle is used in all other functions of this typeclass to perform
  --   Souffle-related actions.
  type Handler m :: Type -> Type

  -- | Helper associated type constraint that allows collecting facts from
  --   Souffle in a list or vector. Only used internally.
  type CollectFacts m (c :: Type -> Type) :: Constraint

  -- | Runs the Souffle program.
  run :: Handler m prog -> m ()

  -- | Sets the number of CPU cores this Souffle program should use.
  setNumThreads :: Handler m prog -> Word64 -> m ()

  -- | Gets the number of CPU cores this Souffle program should use.
  getNumThreads :: Handler m prog -> m Word64

  -- | Returns all facts of a program. This function makes use of type inference
  --   to select the type of fact to return.
  getFacts :: (Fact a, ContainsOutputFact prog a, CollectFacts m c)
           => Handler m prog -> m (c a)

  -- | Searches for a fact in a program.
  --   Returns 'Nothing' if no matching fact was found; otherwise 'Just' the fact.
  --
  --   Conceptually equivalent to @List.find (== fact) \<$\> getFacts prog@,
  --   but this operation can be implemented much faster.
  findFact :: (Fact a, ContainsOutputFact prog a, Eq a)
           => Handler m prog -> a -> m (Maybe a)

  -- | Adds a fact to the program.
  addFact :: (Fact a, ContainsInputFact prog a)
          => Handler m prog -> a -> m ()

  -- | Adds multiple facts to the program. This function could be implemented
  --   in terms of 'addFact', but this is done as a minor optimization.
  addFacts :: (Foldable t, Fact a, ContainsInputFact prog a)
           => Handler m prog -> t a -> m ()

instance MonadSouffle m => MonadSouffle (ReaderT r m) where
  type Handler (ReaderT r m) = Handler m
  type CollectFacts (ReaderT r m) c = CollectFacts m c

  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
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
  type CollectFacts (WriterT w m) c = CollectFacts m c

  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
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
  type CollectFacts (StateT s m) c = CollectFacts m c

  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
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
  type CollectFacts (RWST r w s m) c = CollectFacts m c

  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
  getFacts = lift . getFacts
  {-# INLINABLE getFacts #-}
  findFact prog = lift . findFact prog
  {-# INLINABLE findFact #-}
  addFact fact = lift . addFact fact
  {-# INLINABLE addFact #-}
  addFacts facts = lift . addFacts facts
  {-# INLINABLE addFacts #-}

instance MonadSouffle m => MonadSouffle (ExceptT e m) where
  type Handler (ExceptT e m) = Handler m
  type CollectFacts (ExceptT e m) c = CollectFacts m c

  run = lift . run
  {-# INLINABLE run #-}
  setNumThreads prog = lift . setNumThreads prog
  {-# INLINABLE setNumThreads #-}
  getNumThreads = lift . getNumThreads
  {-# INLINABLE getNumThreads #-}
  getFacts = lift . getFacts
  {-# INLINABLE getFacts #-}
  findFact prog = lift . findFact prog
  {-# INLINABLE findFact #-}
  addFact fact = lift . addFact fact
  {-# INLINABLE addFact #-}
  addFacts facts = lift . addFacts facts
  {-# INLINABLE addFacts #-}


-- | A mtl-style typeclass for Souffle-related actions that involve file IO.
class MonadSouffle m => MonadSouffleFileIO m where
  -- | Load all facts from files in a certain directory.
  loadFiles :: Handler m prog -> FilePath -> m ()

  -- | Write out all facts of the program to CSV files in a certain directory
  --   (as defined in the Souffle program).
  writeFiles :: Handler m prog -> FilePath -> m ()

instance MonadSouffleFileIO m => MonadSouffleFileIO (ReaderT r m) where
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles prog = lift . writeFiles prog
  {-# INLINABLE writeFiles #-}

instance (Monoid w, MonadSouffleFileIO m) => MonadSouffleFileIO (WriterT w m) where
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles prog = lift . writeFiles prog
  {-# INLINABLE writeFiles #-}

instance MonadSouffleFileIO m => MonadSouffleFileIO (StateT s m) where
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles prog = lift . writeFiles prog
  {-# INLINABLE writeFiles #-}

instance (MonadSouffleFileIO m, Monoid w) => MonadSouffleFileIO (RWST r w s m) where
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles prog = lift . writeFiles prog
  {-# INLINABLE writeFiles #-}

instance MonadSouffleFileIO m => MonadSouffleFileIO (ExceptT s m) where
  loadFiles prog = lift . loadFiles prog
  {-# INLINABLE loadFiles #-}
  writeFiles prog = lift . writeFiles prog
  {-# INLINABLE writeFiles #-}
