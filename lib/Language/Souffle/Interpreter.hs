
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DerivingVia, InstanceSigs, UndecidableInstances, BangPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving, DefaultSignatures, DeriveGeneric, LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Language.Souffle.Interpreter
  ( Program(..)
  , Fact(..)
  , Marshal(..)
  , Handle
  , ContainsFact
  , MonadSouffle(..)
  , SouffleM
  , runSouffle
  , cleanUp
  ) where

import Prelude hiding (init)

import Control.Monad.State.Strict
import Data.IORef
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
    )


-- | The handle for the interpreter is the path where the souffle executable
-- can be found, and a template directory where the program is stored.
data HandleData prog = HandleData
  { soufflePath :: FilePath
  , basePath    :: FilePath
  , factPath    :: FilePath
  , outputPath  :: FilePath
  , datalogExec :: FilePath
  , noOfThreads :: Word64
  }

newtype Handle prog = Handle (IORef (HandleData prog))

instance MonadSouffle SouffleM where
  type Handler SouffleM = Handle

  {- | Looks for a souffle interpreter.

     The action will return 'Nothing' if it failed to load the Souffle program.
     Otherwise it will return a 'Handle' that can be used in other functions
     in this module.
  -}
  init :: forall prog . Program prog => prog -> SouffleM (Maybe (Handle prog))
  init prg = SouffleM $ do
    -- TODO: Use system commands to locate the interpreter
    -- TODO: Use system's tmp directory
    tmpDir <- getCanonicalTemporaryDirectory
    souffleTempDir <- createTempDirectory tmpDir "souffle"
    let factDir = souffleTempDir </> "fact"
    let outDir  = souffleTempDir </> "out"
    createDirectoryIfMissing True $ factDir
    createDirectoryIfMissing True $ outDir
    datalogExecutable <- datalogProgramFile prg
    souffleBin  <- souffleCmd
    fmap (Just . Handle) $ newIORef $ HandleData
      { soufflePath = souffleBin
      , basePath    = souffleTempDir
      , factPath    = factDir
      , outputPath  = outDir
      , datalogExec = datalogExecutable
      , noOfThreads = 1
      }

  -- | Runs the Souffle program.
  run :: Handle prog -> SouffleM ()
  run (Handle ref) = SouffleM $ do
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
  setNumThreads :: Handle prog -> Word64 -> SouffleM ()
  setNumThreads (Handle ref) n = SouffleM $ liftIO $
    modifyIORef' ref (\h -> h { noOfThreads = n })

  -- | Gets the number of CPU cores this Souffle program should use.
  getNumThreads :: Handle prog -> SouffleM Word64
  getNumThreads (Handle ref) = SouffleM $ liftIO $
    noOfThreads <$> readIORef ref

  -- | Load all facts from files in a certain directory.
  loadFiles :: Handle prog -> FilePath -> SouffleM ()
  loadFiles (Handle ref) srcPath = SouffleM $ liftIO $do
    handle  <- readIORef ref
    let destPath = factPath handle
    copyFiles srcPath destPath

  -- | Write out all facts of the program to CSV files
  --   (as defined in the Souffle program).
  -- TODO: This should be moved out from this class
  writeFiles :: Handle prog -> {- FilePath -> -} SouffleM ()
  writeFiles (Handle ref) {-destPath-} = SouffleM $ liftIO $do
    handle <- readIORef ref
    let srcPath = factPath handle
    copyFiles srcPath "./facts"

  -- | Returns all facts of a program. This function makes use of type inference
  --   to select the type of fact to return.
  getFacts :: forall a prog . (Marshal a, Fact a, ContainsFact prog a)
           => Handle prog -> SouffleM [a]
  getFacts (Handle ref) = SouffleM $ liftIO $do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = outputPath handle </> relationName <.> "csv"
    factLines <- readCSVFile factFile
    let facts = map (runMarshallIT pop) factLines
    pure facts

  -- | Searches for a fact in a program.
  --   Returns 'Nothing' if no matching fact was found; otherwise 'Just' the fact.
  --
  --   Conceptually equivalent to @List.find (== fact) \<$\> getFacts prog@, but this operation
  --   can be implemented much faster.
  findFact :: (Fact a, ContainsFact prog a, Eq a, Marshal a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact prog fact = find (== fact) <$> getFacts prog

  -- | Adds a fact to the program.
  addFact :: forall a prog . (Fact a, ContainsFact prog a, Marshal a)
          => Handle prog -> a -> SouffleM ()
  addFact (Handle ref) fact = SouffleM $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "csv"
    let line = pushMarshallIT (push fact)
    appendFile factFile $ intercalate "\t" line ++ "\n"

  -- | Adds multiple facts to the program. This function could be implemented
  --   in terms of 'addFact', but this is done as a minor optimization.
  addFacts :: forall a prog f . (Fact a, ContainsFact prog a, Marshal a, Foldable f)
           => Handle prog -> f a -> SouffleM ()
  addFacts (Handle ref) facts = SouffleM $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "csv"
    let factLines :: [[String]] = map (pushMarshallIT . push) (foldMap pure facts)
    mapM_ (\line -> appendFile factFile (intercalate "\t" line ++ "\n")) factLines

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles srcPath destPath = do
  content <- listDirectory srcPath
  forM_ content $ \file -> do
    doesFileExist (srcPath </> file) >>= \case
      False -> pure () -- Dir or symlink, but not file
      True  -> copyFile (srcPath </> file) (destPath </> file)

datalogProgramFile :: forall prog . Program prog => prog -> IO FilePath
datalogProgramFile _prg = do
  dir <- fromMaybe "." <$> lookupEnv "DATALOG_DIR"
  pure $ dir </> programName (Proxy :: Proxy prog) <.> "dl"

souffleCmd :: IO FilePath
souffleCmd = fromMaybe "souffle" <$> lookupEnv "SOUFFLE_BIN"

readCSVFile :: FilePath -> IO [[String]]
readCSVFile = fmap (fmap (splitOn "\t") . lines) . readFile

cleanUp :: forall prog . (Program prog) => Handle prog -> SouffleM ()
cleanUp (Handle ref) = SouffleM $ do
  handle <- readIORef ref
  removeDirectoryRecursive $ factPath    handle
  removeDirectoryRecursive $ outputPath  handle
  removeDirectoryRecursive $ basePath    handle

-- * Marshaling

newtype IMarshal a = IMarshal (State [String] a)
  deriving
    ( Functor
    , Applicative
    , Monad
    )

marshalAlgM :: forall a . MarshalF a -> IMarshal a
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
  state (\st -> ((), show i:st))
  pure v
marshalAlgM (PushStr s v) = IMarshal $ do
  state (\st -> ((), s:st))
  pure v

runMarshallIT :: forall a . MarshalM a -> [String] -> a
runMarshallIT free = calc (interpret marshalAlgM free)
  where
    calc :: IMarshal a -> [String] -> a
    calc (IMarshal m) s = evalState m s

pushMarshallIT :: forall a . MarshalM a -> [String]
pushMarshallIT = calc . interpret marshalAlgM
  where
    calc :: IMarshal a -> [String]
    calc (IMarshal m) = execState m []
