
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DerivingVia, InstanceSigs, UndecidableInstances, BangPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving, DefaultSignatures, DeriveGeneric, LambdaCase #-}
module Language.Souffle.Interpreter
  ( SouffleM
  , runSouffleM
  , init
  , cleanUp
  , run
  , setNumThreads
  , getNumThreads
  , loadFiles
  , writeFiles
  , getFacts
  , findFact
  , addFact
  , addFacts
  , IMarshal
  ) where

import Prelude hiding (init)

import Control.Monad.State.Strict
import Data.IORef
import Data.Int
import Data.Proxy
import Data.Word
import GHC.Generics
import Language.Souffle (ContainsFact, Fact(..), Program(..), CollectFacts, Marshal)
import System.Directory
import System.FilePath
import System.Process
import Text.Printf
import Control.Monad.IO.Class
import Data.List hiding (init)
import Data.List.Extra (splitOn)
import System.IO.Temp
import System.Environment
import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Souffle.Internal.Constraints as C



newtype SouffleM a = SouffleM { runSouffleM :: IO a }
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
  , datalogFile :: FilePath
  , noOfThreads :: Word64
  }

newtype Handle prog = Handle (IORef (HandleData prog))

datalogProgramFile :: forall prog . Program prog => prog -> IO FilePath
datalogProgramFile prg = do
  dir <- fromMaybe "." <$> lookupEnv "DATALOG_DIR"
  pure $ dir </> programName (Proxy :: Proxy prog) <.> "dl"

souffleCmd :: IO FilePath
souffleCmd = fromMaybe "souffle" <$> lookupEnv "SOUFFLE_BIN"

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
  datalogFile <- datalogProgramFile prg
  souffleBin  <- souffleCmd
  fmap (Just . Handle) $ newIORef $ HandleData
    { soufflePath = souffleBin
    , basePath    = souffleTempDir
    , factPath    = factDir
    , outputPath  = outDir
    , datalogFile = datalogFile
    , noOfThreads = 1
    }

cleanUp :: forall prog . Program prog => Handle prog -> SouffleM ()
cleanUp (Handle ref) = SouffleM $ do
  handle <- readIORef ref
  removeDirectoryRecursive $ factPath    handle
  removeDirectoryRecursive $ outputPath  handle
  removeDirectoryRecursive $ basePath    handle

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
      (datalogFile handle)

-- | Sets the number of CPU cores this Souffle program should use.
setNumThreads :: Handle prog -> Word64 -> SouffleM ()
setNumThreads (Handle ref) n = SouffleM $
  modifyIORef' ref (\h -> h { noOfThreads = n })

-- | Gets the number of CPU cores this Souffle program should use.
getNumThreads :: Handle prog -> SouffleM Word64
getNumThreads (Handle ref) = SouffleM $
  noOfThreads <$> readIORef ref

copyFiles :: FilePath -> FilePath -> IO ()
copyFiles srcPath destPath = do
  content <- listDirectory srcPath
  forM_ content $ \file -> do
    doesFileExist (srcPath </> file) >>= \case
      False -> pure () -- Dir or symlink, but not file
      True  -> copyFile (srcPath </> file) (destPath </> file)

-- | Load all facts from files in a certain directory.
loadFiles :: Handle prog -> FilePath -> SouffleM ()
loadFiles (Handle ref) srcPath = SouffleM $ do
  handle  <- readIORef ref
  let destPath = factPath handle
  copyFiles srcPath destPath

-- | Write out all facts of the program to CSV files
--   (as defined in the Souffle program).
writeFiles :: Handle prog -> FilePath -> SouffleM ()
writeFiles (Handle ref) destPath = SouffleM $ do
  handle <- readIORef ref
  let srcPath = factPath handle
  copyFiles srcPath destPath

readCSVFile :: FilePath -> IO [[String]]
readCSVFile = fmap (fmap (splitOn "\t") . lines) . readFile

-- | Returns all facts of a program. This function makes use of type inference
--   to select the type of fact to return.
getFacts :: forall a prog c . (IMarshal a, Fact a, ContainsFact prog a)
         => Handle prog -> SouffleM [a]
getFacts (Handle ref) = SouffleM $ do
  handle <- readIORef ref
  let relationName = factName (Proxy :: Proxy a)
  let factFile = outputPath handle </> relationName <.> "csv"
  factLines <- readCSVFile factFile
  facts <- mapM (runMarshallIT pop) factLines
  pure facts

-- | Searches for a fact in a program.
--   Returns 'Nothing' if no matching fact was found; otherwise 'Just' the fact.
--
--   Conceptually equivalent to @List.find (== fact) \<$\> getFacts prog@, but this operation
--   can be implemented much faster.
findFact :: (Fact a, ContainsFact prog a, Eq a, IMarshal a)
         => Handle prog -> a -> SouffleM (Maybe a)
findFact prog fact = find (== fact) <$> getFacts prog

-- | Adds a fact to the program.
addFact :: forall a prog . (Fact a, ContainsFact prog a, IMarshal a)
        => Handle prog -> a -> SouffleM ()
addFact (Handle ref) fact = SouffleM $ do
  handle <- readIORef ref
  let relationName = factName (Proxy :: Proxy a)
  let factFile = factPath handle </> relationName <.> "csv"
  line <- pushMarshallIT (push fact)
  appendFile factFile $ intercalate "\t" line ++ "\n"

-- | Adds multiple facts to the program. This function could be implemented
--   in terms of 'addFact', but this is done as a minor optimization.
addFacts :: forall a prog . (Fact a, ContainsFact prog a, IMarshal a)
         => Handle prog -> [a] -> SouffleM ()
addFacts (Handle ref) facts = SouffleM $ do
  handle <- readIORef ref
  let relationName = factName (Proxy :: Proxy a)
  let factFile = factPath handle </> relationName <.> "csv"
  lines <- mapM (pushMarshallIT . push) facts
  mapM_ (\line -> appendFile factFile (intercalate "\t" line ++ "\n")) lines

newtype IMarshalT m a = IMarshalT (StateT [String] m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    )

runMarshallIT :: Monad m => IMarshalT m a -> [String] -> m a
runMarshallIT (IMarshalT m) s = evalStateT m s

pushMarshallIT :: Monad m => IMarshalT m a -> m [String]
pushMarshallIT (IMarshalT m) = execStateT m []

class IMarshal a where
  -- | Marshals a value to the datalog side.
  push :: Monad m => a -> IMarshalT m ()
  -- | Unmarshals a value from the datalog side.
  pop :: Monad m => IMarshalT m a

  default push :: (Generic a, C.SimpleProduct a (Rep a), IGMarshal (Rep a), Monad m)
               => a -> IMarshalT m ()
  default pop :: (Generic a, C.SimpleProduct a (Rep a), IGMarshal (Rep a), Monad m)
              => IMarshalT m a
  push a = gpush (from a)
  {-# INLINABLE push #-}
  pop = to <$> gpop
  {-# INLINABLE pop #-}

instance IMarshal Int32 where
  push int = IMarshalT (modify' (show int:))
  {-# INLINABLE push #-}
  pop = IMarshalT $ state (\(h:t) -> (read h, t))
  {-# INLINABLE pop #-}

instance IMarshal String where
  push str = IMarshalT $ modify' (str:)
  {-# INLINABLE push #-}
  pop = IMarshalT $ state (\(h:t) -> (h,t))
  {-# INLINABLE pop #-}

instance IMarshal T.Text where
  push = (push . T.unpack)
  {-# INLINABLE push #-}
  pop = (T.pack <$> pop)
  {-# INLINABLE pop #-}

instance IMarshal TL.Text where
  push = (push . TL.unpack)
  {-# INLINABLE push #-}
  pop = (TL.pack <$> pop)
  {-# INLINABLE pop #-}

class IGMarshal f where
  gpush :: Monad m => f a -> IMarshalT m ()
  gpop :: Monad m => IMarshalT m (f a)

instance IMarshal a => IGMarshal (K1 i a) where
  gpush (K1 x) = push x
  {-# INLINABLE gpush #-}
  gpop = K1 <$> pop
  {-# INLINABLE gpop #-}

instance (IGMarshal f, IGMarshal g) => IGMarshal (f :*: g) where
  gpush (a :*: b) = do
    gpush a
    gpush b
  {-# INLINABLE gpush #-}
  gpop = (:*:) <$> gpop <*> gpop
  {-# INLINABLE gpop #-}

instance IGMarshal a => IGMarshal (M1 i c a) where
  gpush (M1 x) = gpush x
  {-# INLINABLE gpush #-}
  gpop = M1 <$> gpop
  {-# INLINABLE gpop #-}
