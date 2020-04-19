{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies, DerivingVia, InstanceSigs #-}

-- | This module provides an implementation for the `MonadSouffle` typeclass
--   defined in "Language.Souffle.Class".
--   It makes use of the Souffle interpreter and CSV files to offer an
--   implementation optimized for quick development speed compared to
--   "Language.Souffle.Compiled".
--
--   It is however __much__ slower so users are advised to switch over to
--   the compiled alternative once the prototyping phase is finished.
module Language.Souffle.Interpreted
  ( Program(..)
  , Fact(..)
  , Marshal(..)
  , Handle
  , SouffleM
  , runSouffle
  , runSouffleWith
  , cleanup
  ) where

import Prelude hiding (init)

import Control.DeepSeq (deepseq)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.IORef
import Data.Foldable (traverse_)
import Data.List hiding (init)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Vector as V
import Data.Word
import Language.Souffle.Class
import Language.Souffle.Marshal
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO (hGetContents)
import System.IO.Temp
import System.Process
import Text.Printf


-- | A monad for executing Souffle-related actions in.
newtype SouffleM a
  = SouffleM (ReaderT (Maybe FilePath) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  via (ReaderT (Maybe FilePath) IO)

-- | Returns the underlying IO action.
runSouffle :: SouffleM a -> IO a
runSouffle (SouffleM m) = runReaderT m Nothing

-- | Run a souffle interpreter that will look up the datalog program
--   in the given directory.
runSouffleWith :: FilePath -> SouffleM a -> IO a
runSouffleWith datalogProgramPath (SouffleM m) = runReaderT m (Just datalogProgramPath)

-- | A datatype representing a handle to a datalog program.
--   The type parameter is used for keeping track of which program
--   type the handle belongs to for additional type safety.
newtype Handle prog = Handle (IORef HandleData)

-- | The data needed for the interpreter is the path where the souffle
--   executable can be found, and a template directory where the program
--   is stored.
data HandleData = HandleData
  { soufflePath :: FilePath
  , basePath    :: FilePath
  , factPath    :: FilePath
  , outputPath  :: FilePath
  , datalogExec :: FilePath
  , noOfThreads :: Word64
  }

newtype IMarshal a = IMarshal (State [String] a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState [String]
    )
  via (State [String])

popMarshalT :: MarshalM PopF a -> [String] -> a
popMarshalT = runM . interpret popAlgM where
  runM (IMarshal m) = evalState m
  popAlgM (PopStr f) = do
    str <- state (\case
              [] -> error "Empty fact stack"
              (h:t) -> (h, t))
    pure $ f str
  popAlgM (PopInt f) = do
    int <- state (\case
              [] -> error "Empty fact stack"
              (h:t) -> (read h, t))
    pure $ f int
{-# INLINABLE popMarshalT #-}

pushMarshalT :: MarshalM PushF a -> [String]
pushMarshalT = runM . interpret pushAlgM where
  runM (IMarshal m) = reverse $ execState m []
  pushAlgM (PushInt i v) = do
    modify (show i:)
    pure v
  pushAlgM (PushStr s v) = do
    modify (s:)
    pure v
{-# INLINABLE pushMarshalT #-}


class Collect c where
  collect :: Marshal a => FilePath -> IO (c a)

instance Collect [] where
  collect factFile = do
    factLines <- readCSVFile factFile
    let facts = map (popMarshalT pop) factLines
    pure $! facts
  {-# INLINABLE collect #-}

instance Collect V.Vector where
  collect factFile = V.fromList <$!> collect factFile
  {-# INLINABLE collect #-}

instance MonadSouffle SouffleM where
  type Handler SouffleM = Handle
  type CollectFacts SouffleM c = Collect c

  {- | Looks for a souffle interpreter.

     The action will return 'Nothing' if it failed to find the Souffle interpreter and
     load the Souffle program.
     Otherwise it will return a 'Handle' that can be used in other functions
     in this module.
  -}
  init :: forall prog. Program prog => prog -> SouffleM (Maybe (Handle prog))
  init prg = SouffleM $ datalogProgramFile prg >>= \case
    Nothing -> pure Nothing
    Just datalogExecutable -> liftIO $ do
      tmpDir <- getCanonicalTemporaryDirectory
      souffleTempDir <- createTempDirectory tmpDir "souffle-haskell"
      let factDir = souffleTempDir </> "fact"
      let outDir  = souffleTempDir </> "out"
      createDirectoryIfMissing True factDir
      createDirectoryIfMissing True outDir
      mSouffleBin <- mplus <$> lookupEnv "SOUFFLE_BIN"
                           <*> locateSouffle
      forM mSouffleBin $ \souffleBin ->
        fmap Handle $ newIORef $ HandleData
          { soufflePath = souffleBin
          , basePath    = souffleTempDir
          , factPath    = factDir
          , outputPath  = outDir
          , datalogExec = datalogExecutable
          , noOfThreads = 1
          }
  {-# INLINABLE init #-}

  -- | Runs the Souffle program.
  run (Handle ref) = liftIO $ do
    handle <- readIORef ref
    -- Invoke the souffle binary using parameters, supposing that the facts are placed
    -- in the factPath, rendering the output into the outputPath
    callCommand $
      printf "%s -F%s -D%s -j%d %s"
        (soufflePath handle)
        (factPath handle)
        (outputPath handle)
        (noOfThreads handle)
        (datalogExec handle)
  {-# INLINABLE run #-}

  -- | Sets the number of CPU cores this Souffle program should use.
  setNumThreads (Handle ref) n = liftIO $
    modifyIORef' ref (\h -> h { noOfThreads = n })
  {-# INLINABLE setNumThreads #-}

  -- | Gets the number of CPU cores this Souffle program should use.
  getNumThreads (Handle ref) = liftIO $
    noOfThreads <$> readIORef ref
  {-# INLINABLE getNumThreads #-}

  -- | Returns all facts of a program. This function makes use of type inference
  --   to select the type of fact to return.
  getFacts :: forall a c prog. (Marshal a, Fact a, ContainsFact prog a, Collect c)
           => Handle prog -> SouffleM (c a)
  getFacts (Handle ref) = liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = outputPath handle </> relationName <.> "csv"
    facts <- collect factFile
    pure $! facts  -- force facts before running to avoid issues with lazy IO
  {-# INLINABLE getFacts #-}

  -- | Searches for a fact in a program.
  --   Returns 'Nothing' if no matching fact was found; otherwise 'Just' the fact.
  --
  --   Conceptually equivalent to @List.find (== fact) \<$\> getFacts prog@, but this operation
  --   can be implemented much faster.
  findFact :: (Fact a, ContainsFact prog a, Eq a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact prog fact = do
    facts :: [a] <- getFacts prog
    pure $ find (== fact) facts
  {-# INLINABLE findFact #-}

  -- | Adds a fact to the program.
  addFact :: forall a prog. (Fact a, ContainsFact prog a, Marshal a)
          => Handle prog -> a -> SouffleM ()
  addFact (Handle ref) fact = liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "facts"
    let line = pushMarshalT (push fact)
    appendFile factFile $ intercalate "\t" line ++ "\n"
  {-# INLINABLE addFact #-}

  -- | Adds multiple facts to the program. This function could be implemented
  --   in terms of 'addFact', but this is done as a minor optimization.
  addFacts :: forall a prog f. (Fact a, ContainsFact prog a, Marshal a, Foldable f)
           => Handle prog -> f a -> SouffleM ()
  addFacts (Handle ref) facts = SouffleM $ liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "facts"
    let factLines = map (pushMarshalT . push) (foldMap pure facts)
    traverse_ (\line -> appendFile factFile (intercalate "\t" line ++ "\n")) factLines
  {-# INLINABLE addFacts #-}

datalogProgramFile :: forall prog. Program prog => prog -> ReaderT (Maybe FilePath) IO (Maybe FilePath)
datalogProgramFile _ =
  asks mplus
        <*> liftIO (do
              dir <- fromMaybe "." <$> lookupEnv "DATALOG_DIR"
              let dlFile = dir </> programName (Proxy :: Proxy prog) <.> "dl"
              doesFileExist dlFile >>= \case
                False -> pure Nothing
                True -> pure $ Just dlFile)
{-# INLINABLE datalogProgramFile #-}

locateSouffle :: IO (Maybe FilePath)
locateSouffle = do
  let locateCmd = (shell "whereis souffle")
                    { std_out = CreatePipe
                    }
  (_, Just hout, _, locateCmdHandle) <- createProcess locateCmd
  waitForProcess locateCmdHandle >>= \case
    ExitFailure _ -> pure Nothing
    ExitSuccess   ->
      fmap words (hGetContents hout) >>= \case
        (_souffle : souffleBin : _) -> pure (souffleBin `deepseq` Just souffleBin)
        _                           -> pure Nothing
{-# INLINABLE locateSouffle #-}

readCSVFile :: FilePath -> IO [[String]]
readCSVFile path = doesFileExist path >>= \case
  False -> pure []
  True -> do
    contents <- readFile path
    -- deepseq needed to avoid issues with lazy IO
    pure $ contents `deepseq` (map (splitOn '\t') . lines) contents
{-# INLINABLE readCSVFile #-}

-- | Cleans up the temporary directory that this library has written files to.
--   This functionality is only provided for the interpreted version since the
--   compiled version directly (de-)serializes data via the C++ API.
cleanup :: forall prog. Program prog => Handle prog -> SouffleM ()
cleanup (Handle ref) = liftIO $ do
  handle <- readIORef ref
  traverse_ removeDirectoryRecursive [factPath handle, outputPath handle, basePath handle]
{-# INLINABLE cleanup #-}

splitOn :: Char -> String -> [String]
splitOn c s =
  let (x, rest) = break (== c) s
      rest' = drop 1 rest
   in x : splitOn c rest'
{-# INLINABLE splitOn #-}

