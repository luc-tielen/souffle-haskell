{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DataKinds, FlexibleContexts, TypeFamilies, DerivingVia, InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

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
  , Config(..)
  , Handle
  , SouffleM
  , MonadSouffle(..)
  , runSouffle
  , runSouffleWith
  , defaultConfig
  , cleanup
  ) where

import Prelude hiding (init)

import Control.DeepSeq (deepseq)
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.IORef
import Data.Foldable (traverse_)
import Data.List hiding (init)
import Data.Semigroup (Last(..))
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
  = SouffleM (ReaderT Config IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  via (ReaderT Config IO)

-- | A helper data type for storing the configurable settings of the
--   interpreter.
--
--   - __cfgDatalogDir__: The directory where the datalog file(s) are located.
--   - __cfgSouffleBin__: The name of the souffle binary. Has to be available in
--   \$PATH or an absolute path needs to be provided. Note: Passing in `Nothing`
--   will fail to start up the interpreter in the `MonadSouffle.init` function.
data Config
  = Config
  { cfgDatalogDir :: FilePath
  , cfgSouffleBin :: Maybe FilePath
  } deriving Show

-- | Retrieves the default config for the interpreter. These settings can
--   be overridden using record update syntax if needed.
--
--   By default, the settings will be configured as follows:
--
--   - __cfgDatalogDir__: Looks at environment variable \$DATALOG_DIR,
--   falls back to the current directory if not set.
--   - __cfgSouffleBin__: Looks at environment variable \$SOUFFLE_BIN,
--   or tries to locate the souffle binary using the which shell command
--   if the variable is not set.
defaultConfig :: MonadIO m => m Config
defaultConfig = liftIO $ do
  dlDir <- lookupEnv "DATALOG_DIR"
  envSouffleBin <- fmap Last <$> lookupEnv "SOUFFLE_BIN"
  locatedBin <- fmap Last <$> locateSouffle
  let souffleBin = getLast <$> locatedBin <> envSouffleBin
  pure $ Config (fromMaybe "." dlDir) souffleBin
{-# INLINABLE defaultConfig #-}

-- | Returns an IO action that will run the Souffle interpreter with
--   default settings (see `defaultConfig`).
runSouffle :: SouffleM a -> IO a
runSouffle m = do
  cfg <- defaultConfig
  runSouffleWith cfg m
{-# INLINABLE runSouffle #-}

-- | Returns an IO action that will run the Souffle interpreter with
--   the given interpreter settings.
runSouffleWith :: Config -> SouffleM a -> IO a
runSouffleWith cfg (SouffleM m) = runReaderT m cfg
{-# INLINABLE runSouffleWith #-}

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

newtype IMarshal (d :: Direction) a = IMarshal (State [String] a)
  deriving (Functor, Applicative, Monad, MonadState [String])
  via (State [String])

instance MonadMarshal d IMarshal where
  pushInt int = modify (show int:)
  {-# INLINABLE pushInt #-}

  pushString str = modify (str:)
  {-# INLINABLE pushString #-}

  popInt = state $ \case
    [] -> error "Empty fact stack"
    (h:t) -> (read h, t)
  {-# INLINABLE popInt #-}

  popString = state $ \case
    [] -> error "Empty fact stack"
    (h:t) -> (h, t)
  {-# INLINABLE popString #-}

popMarshalT :: IMarshal 'Pop a -> [String] -> a
popMarshalT (IMarshal m) = evalState m
{-# INLINABLE popMarshalT #-}

pushMarshalT :: IMarshal 'Push a -> [String]
pushMarshalT (IMarshal m) = reverse $ execState m []
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

  init :: forall prog. Program prog => prog -> SouffleM (Maybe (Handle prog))
  init prg = SouffleM $ datalogProgramFile prg >>= \case
    Nothing -> pure Nothing
    Just datalogExecutable -> do
      souffleTempDir <- liftIO $ do
        tmpDir <- getCanonicalTemporaryDirectory
        createTempDirectory tmpDir "souffle-haskell"
      let factDir = souffleTempDir </> "fact"
          outDir  = souffleTempDir </> "out"
      liftIO $ do
        createDirectoryIfMissing True factDir
        createDirectoryIfMissing True outDir
      mSouffleBin <- asks cfgSouffleBin
      liftIO $ forM mSouffleBin $ \souffleBin ->
        fmap Handle $ newIORef $ HandleData
          { soufflePath = souffleBin
          , basePath    = souffleTempDir
          , factPath    = factDir
          , outputPath  = outDir
          , datalogExec = datalogExecutable
          , noOfThreads = 1
          }
  {-# INLINABLE init #-}

  run (Handle ref) = liftIO $ do
    handle <- readIORef ref
    -- Invoke the souffle binary using parameters, supposing that the facts
    -- are placed in the factPath, rendering the output into the outputPath.
    callCommand $
      printf "%s -F%s -D%s -j%d %s"
        (soufflePath handle)
        (factPath handle)
        (outputPath handle)
        (noOfThreads handle)
        (datalogExec handle)
  {-# INLINABLE run #-}

  setNumThreads (Handle ref) n = liftIO $
    modifyIORef' ref (\h -> h { noOfThreads = n })
  {-# INLINABLE setNumThreads #-}

  getNumThreads (Handle ref) = liftIO $
    noOfThreads <$> readIORef ref
  {-# INLINABLE getNumThreads #-}

  getFacts :: forall a c prog. (Marshal a, Fact a, ContainsFact prog a, Collect c)
           => Handle prog -> SouffleM (c a)
  getFacts (Handle ref) = liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = outputPath handle </> relationName <.> "csv"
    facts <- collect factFile
    pure $! facts  -- force facts before running to avoid issues with lazy IO
  {-# INLINABLE getFacts #-}

  findFact :: (Fact a, ContainsFact prog a, Eq a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact prog fact = do
    facts :: [a] <- getFacts prog
    pure $ find (== fact) facts
  {-# INLINABLE findFact #-}

  addFact :: forall a prog. (Fact a, ContainsFact prog a, Marshal a)
          => Handle prog -> a -> SouffleM ()
  addFact (Handle ref) fact = liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "facts"
    let line = pushMarshalT (push fact)
    appendFile factFile $ intercalate "\t" line ++ "\n"
  {-# INLINABLE addFact #-}

  addFacts :: forall a prog f. (Fact a, ContainsFact prog a, Marshal a, Foldable f)
           => Handle prog -> f a -> SouffleM ()
  addFacts (Handle ref) facts = SouffleM $ liftIO $ do
    handle <- readIORef ref
    let relationName = factName (Proxy :: Proxy a)
    let factFile = factPath handle </> relationName <.> "facts"
    let factLines = map (pushMarshalT . push) (foldMap pure facts)
    traverse_ (\line -> appendFile factFile (intercalate "\t" line ++ "\n")) factLines
  {-# INLINABLE addFacts #-}

datalogProgramFile :: forall prog. Program prog => prog -> ReaderT Config IO (Maybe FilePath)
datalogProgramFile _ = do
  dir <- asks cfgDatalogDir
  let dlFile = dir </> programName (Proxy :: Proxy prog) <.> "dl"
  liftIO $ doesFileExist dlFile >>= \case
    False -> pure Nothing
    True -> pure $ Just dlFile
{-# INLINABLE datalogProgramFile #-}

locateSouffle :: IO (Maybe FilePath)
locateSouffle = do
  let locateCmd = (shell "which souffle") { std_out = CreatePipe }
  (_, Just hout, _, locateCmdHandle) <- createProcess locateCmd
  waitForProcess locateCmdHandle >>= \case
    ExitFailure _ -> pure Nothing
    ExitSuccess ->
      words <$> hGetContents hout >>= \case
        [souffleBin] -> pure $ Just souffleBin
        _ -> pure Nothing
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

