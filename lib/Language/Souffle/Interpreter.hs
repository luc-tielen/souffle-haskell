
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeFamilies, DerivingVia, InstanceSigs #-}
module Language.Souffle.Interpreter
  ( Program(..)
  , Fact(..)
  , Marshal(..)
  , Handle
  , ContainsFact
  , MonadSouffle(..)
  , SouffleM
  , runSouffle
  , cleanup
  ) where

import Prelude hiding (init)

import Control.DeepSeq (deepseq)
import Control.Monad.State.Strict
import Data.IORef
import Data.Foldable (traverse_)
import Data.List hiding (init)
import Data.List.Extra (splitOn)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Word
import Language.Souffle.Class
import Language.Souffle.Marshal
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Temp
import System.Process
import Text.Printf


newtype SouffleM a = SouffleM { runSouffle :: IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MonadIO
    ) via IO


-- | The handle for the interpreter is the path where the souffle executable
-- can be found, and a template directory where the program is stored.
data HandleData = HandleData
  { soufflePath :: FilePath
  , basePath    :: FilePath
  , factPath    :: FilePath
  , outputPath  :: FilePath
  , datalogExec :: FilePath
  , noOfThreads :: Word64
  }

newtype Handle prog = Handle (IORef HandleData)

instance MonadSouffle SouffleM where
  type Handler SouffleM = Handle

  {- | Looks for a souffle interpreter.

     The action will return 'Nothing' if it failed to load the Souffle program.
     Otherwise it will return a 'Handle' that can be used in other functions
     in this module.
  -}
  init :: forall prog. Program prog => prog -> SouffleM (Maybe (Handle prog))
  init prg = SouffleM $ datalogProgramFile prg >>= \case
    Nothing -> pure Nothing
    Just datalogExecutable -> do
      tmpDir <- getCanonicalTemporaryDirectory
      souffleTempDir <- createTempDirectory tmpDir "souffle-haskell"
      let factDir = souffleTempDir </> "fact"
      let outDir  = souffleTempDir </> "out"
      createDirectoryIfMissing True factDir
      createDirectoryIfMissing True outDir
      -- TODO: Use system commands to locate the interpreter
      -- TODO: souffle not found => Nothing
      souffleBin <- fromMaybe "souffle" <$> lookupEnv "SOUFFLE_BIN"
      fmap (Just . Handle) $ newIORef $ HandleData
        { soufflePath = souffleBin
        , basePath    = souffleTempDir
        , factPath    = factDir
        , outputPath  = outDir
        , datalogExec = datalogExecutable
        , noOfThreads = 1
        }

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

  -- | Sets the number of CPU cores this Souffle program should use.
  setNumThreads (Handle ref) n = liftIO $
    modifyIORef' ref (\h -> h { noOfThreads = n })

  -- | Gets the number of CPU cores this Souffle program should use.
  getNumThreads (Handle ref) = liftIO $
    noOfThreads <$> readIORef ref

  -- | Returns all facts of a program. This function makes use of type inference
  --   to select the type of fact to return.
  getFacts :: forall a prog. (Marshal a, Fact a, ContainsFact prog a)
           => Handle prog -> SouffleM [a]
  getFacts (Handle ref) = liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = outputPath handle </> relationName <.> "csv"
    factLines <- readCSVFile factFile
    let facts = map (runMarshallIT pop) factLines
    pure $! facts  -- force facts before running to avoid issues with lazy IO

  -- | Searches for a fact in a program.
  --   Returns 'Nothing' if no matching fact was found; otherwise 'Just' the fact.
  --
  --   Conceptually equivalent to @List.find (== fact) \<$\> getFacts prog@, but this operation
  --   can be implemented much faster.
  findFact prog fact = find (== fact) <$> getFacts prog

  -- | Adds a fact to the program.
  addFact :: forall a prog. (Fact a, ContainsFact prog a, Marshal a)
          => Handle prog -> a -> SouffleM ()
  addFact (Handle ref) fact = liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "facts"
    let line = pushMarshallIT (push fact)
    appendFile factFile $ intercalate "\t" line ++ "\n"

  -- | Adds multiple facts to the program. This function could be implemented
  --   in terms of 'addFact', but this is done as a minor optimization.
  addFacts :: forall a prog f. (Fact a, ContainsFact prog a, Marshal a, Foldable f)
           => Handle prog -> f a -> SouffleM ()
  addFacts (Handle ref) facts = SouffleM $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "facts"
    let factLines = map (pushMarshallIT . push) (foldMap pure facts)
    traverse_ (\line -> appendFile factFile (intercalate "\t" line ++ "\n")) factLines

datalogProgramFile :: forall prog. Program prog => prog -> IO (Maybe FilePath)
datalogProgramFile _ = do
  dir <- fromMaybe "." <$> lookupEnv "DATALOG_DIR"
  let dlFile = dir </> programName (Proxy :: Proxy prog) <.> "dl"
  doesFileExist dlFile >>= \case
    False -> pure Nothing
    True -> pure $ Just dlFile

readCSVFile :: FilePath -> IO [[String]]
readCSVFile path = doesFileExist path >>= \case
  False -> pure []
  True -> do
    contents <- readFile path
    -- deepseq needed to avoid issues with lazy IO
    pure $ contents `deepseq` (map (splitOn "\t") . lines) contents

cleanup :: forall prog. Program prog => Handle prog -> SouffleM ()
cleanup (Handle ref) = liftIO $ do
  handle <- readIORef ref
  traverse_ removeDirectoryRecursive [factPath handle, outputPath handle, basePath handle]


newtype IMarshal a = IMarshal (State [String] a)
  deriving
    ( Functor
    , Applicative
    , Monad
    ) via (State [String])

marshalAlgM :: MarshalF a -> IMarshal a
marshalAlgM (PopStr f) = IMarshal $ do
  str <- state (\case
            [] -> error "Empty fact stack"
            (h:t) -> (h, t))
  pure $ f str
marshalAlgM (PopInt f) = IMarshal $ do
  int <- state (\case
            [] -> error "Empty fact stack"
            (h:t) -> (read h, t))
  pure $ f int
marshalAlgM (PushInt i v) = IMarshal $ do
  modify (show i:)
  pure v
marshalAlgM (PushStr s v) = IMarshal $ do
  modify (s:)
  pure v

runMarshallIT :: MarshalM a -> [String] -> a
runMarshallIT = calc . interpret marshalAlgM
  where
    calc :: IMarshal a -> [String] -> a
    calc (IMarshal m) = evalState m

pushMarshallIT :: MarshalM a -> [String]
pushMarshallIT = calc . interpret marshalAlgM
  where
    calc :: IMarshal a -> [String]
    calc (IMarshal m) = reverse $ execState m []
