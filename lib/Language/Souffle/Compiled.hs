{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies, DerivingVia #-}
{-# LANGUAGE BangPatterns, RoleAnnotations, MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs, DataKinds #-}

-- | This module provides an implementation for the typeclasses defined in
--   "Language.Souffle.Class".
--   It makes use of the low level Souffle C++ API to offer a much more
--   performant alternative implementation to the implementation in
--   "Language.Souffle.Interpreted".
--
--   This module is mainly intended to be used after the prototyping phase is
--   over since the iteration cycle is slower due to the additional
--   C++ compilation times.
module Language.Souffle.Compiled
  ( Program(..)
  , Fact(..)
  , Marshal(..)
  , Direction(..)
  , ContainsInputFact
  , ContainsOutputFact
  , Handle
  , SouffleM
  , MonadSouffle(..)
  , MonadSouffleFileIO(..)
  , runSouffle
  ) where

import Prelude hiding ( init )
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable ( traverse_ )
import Data.Functor.Identity
import Data.Proxy
import Data.Monoid
import qualified Data.Array as A
import qualified Data.Array.IO as A
import qualified Data.Array.Unsafe as A
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Text.Foreign as TF
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Foreign.Storable as S
import Language.Souffle.Class
import qualified Language.Souffle.Internal as Internal
import Language.Souffle.Marshal

-- | A datatype representing a handle to a datalog program.
--   The type parameter is used for keeping track of which program
--   type the handle belongs to for additional type safety.
data Handle prog = Handle (ForeignPtr Internal.Souffle)
type role Handle nominal

-- | A monad for executing Souffle-related actions in.
newtype SouffleM a = SouffleM (IO a)
  deriving (Functor, Applicative, Monad, MonadIO) via IO
  deriving (Semigroup, Monoid) via (IO a)

{- | Initializes and runs a Souffle program.

     The 2nd argument is passed in a handle after initialization of the
     Souffle program. The handle will contain 'Nothing' if it failed to
     load the Souffle C++ program. In the successful case it will contain
     a handle that can be used for performing Souffle related actions
     using the other functions in this module.
-}
runSouffle :: forall prog a. Program prog
           => prog -> (Maybe (Handle prog) -> SouffleM a) -> IO a
runSouffle prog action =
  let progName = programName prog
      (SouffleM result) = do
        handle <- fmap Handle <$> liftIO (Internal.init progName)
        action handle
   in result

type ByteBuf = Ptr Internal.ByteBuf

-- | A monad used solely for marshalling and unmarshalling
--   between Haskell and Souffle Datalog.
newtype CMarshal a = CMarshal (StateT ByteBuf IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState ByteBuf)
  via ( StateT ByteBuf IO )

runMarshalM :: CMarshal a -> ByteBuf -> IO a
runMarshalM (CMarshal m) = evalStateT m
{-# INLINABLE runMarshalM #-}

instance MonadPush CMarshal where
  pushInt32 = writeAsBytes
  {-# INLINABLE pushInt32 #-}
  pushUInt32 = writeAsBytes
  {-# INLINABLE pushUInt32 #-}
  pushFloat = writeAsBytes
  {-# INLINABLE pushFloat #-}
  pushString str = do
    pushUInt32 (fromIntegral $ numBytes str)
    traverse_ pokeChar str
    where
      pokeChar c = do
        ptr <- gets castPtr
        liftIO $ S.poke ptr c
        put $ ptr `plusPtr` 1
  {-# INLINABLE pushString #-}
  pushText txt = do
    pushUInt32 (fromIntegral byteCount)
    ptr <- gets castPtr
    liftIO $ TF.unsafeCopyToPtr txt ptr
    put $ ptr `plusPtr` byteCount
    where byteCount = numBytes txt
  {-# INLINABLE pushText #-}

instance MonadPop CMarshal where
  popInt32 = readAsBytes
  {-# INLINABLE popInt32 #-}
  popUInt32 = readAsBytes
  {-# INLINABLE popUInt32 #-}
  popFloat = readAsBytes
  {-# INLINABLE popFloat #-}
  popString = do
    len <- popUInt32
    loop len []
    where
      loop count acc
        | count == 0 = pure $ reverse acc  -- TODO: use difflist?
        | otherwise  = do
          c <- peekChar
          loop (count - 1) (c:acc)
      peekChar = do
        ptr <- gets castPtr
        c <- liftIO $ S.peek ptr
        put $ ptr `plusPtr` 1
        pure c
  {-# INLINABLE popString #-}
  popText = do
    byteCount <- popUInt32
    ptr <- gets castPtr
    let lengthU16 = fromIntegral $ byteCount `div` 2
    txt <- liftIO $ TF.fromPtr ptr lengthU16
    put $ ptr `plusPtr` fromIntegral byteCount
    pure txt
  {-# INLINABLE popText #-}

class Collect c where
  collect :: Marshal a => Word32 -> CMarshal (c a)

instance Collect [] where
  collect objCount
    | objCount == 0 = pure []
    | otherwise =
      (:) <$!> pop <*> collect (objCount - 1)
  {-# INLINABLE collect #-}

instance Collect V.Vector where
  collect objCount = do
    vm <- liftIO $ MV.unsafeNew objCount'
    collect' vm 0
    where
      objCount' = fromIntegral objCount
      collect' vec idx
        | idx == objCount' = liftIO $ V.unsafeFreeze vec
        | otherwise = do
          !obj <- pop
          liftIO $ MV.write vec idx obj
          collect' vec (idx + 1)
  {-# INLINABLE collect #-}

instance Collect (A.Array Int) where
  collect objCount = do
    ma <- liftIO $ A.newArray_ (0, objCount' - 1)
    collect' ma 0
    where
      objCount' = fromIntegral objCount
      collect' :: Marshal a => A.IOArray Int a -> Int -> CMarshal (A.Array Int a)
      collect' array idx
        | idx == objCount' = liftIO $ A.unsafeFreeze array
        | otherwise = do
          !obj <- pop
          liftIO $ A.writeArray array idx obj
          collect' array (idx + 1)
  {-# INLINABLE collect #-}

instance MonadSouffle SouffleM where
  type Handler SouffleM = Handle
  type CollectFacts SouffleM c = Collect c

  run (Handle prog) = SouffleM $ Internal.run prog
  {-# INLINABLE run #-}

  setNumThreads (Handle prog) numCores =
    SouffleM $ Internal.setNumThreads prog numCores
  {-# INLINABLE setNumThreads #-}

  getNumThreads (Handle prog) =
    SouffleM $ Internal.getNumThreads prog
  {-# INLINABLE getNumThreads #-}

  addFact :: forall a prog. (Fact a, ContainsInputFact prog a)
          => Handle prog -> a -> SouffleM ()
  addFact (Handle prog) fact = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    writeBytes relation (Identity fact)
  {-# INLINABLE addFact #-}

  addFacts :: forall t a prog. (Foldable t, Fact a, ContainsInputFact prog a)
           => Handle prog -> t a -> SouffleM ()
  addFacts (Handle prog) facts = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    writeBytes relation facts
  {-# INLINABLE addFacts #-}

  getFacts :: forall a c prog. (Fact a, ContainsOutputFact prog a, Collect c)
           => Handle prog -> SouffleM (c a)
  getFacts (Handle prog) = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    buf <- Internal.popFacts relation
    withForeignPtr buf $ runMarshalM $ collect =<< popUInt32
  {-# INLINABLE getFacts #-}

  findFact :: forall a prog. (Fact a, ContainsOutputFact prog a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact (Handle prog) fact = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    allocaBytes (numBytes fact) $ \ptr -> do
      runMarshalM (push fact) ptr
      found <- Internal.containsFact relation ptr
      pure $ if found then Just fact else Nothing
  {-# INLINABLE findFact #-}

instance MonadSouffleFileIO SouffleM where
  loadFiles (Handle prog) = SouffleM . Internal.loadAll prog
  {-# INLINABLE loadFiles #-}

  writeFiles (Handle prog) = SouffleM . Internal.printAll prog
  {-# INLINABLE writeFiles #-}


writeBytes :: (Foldable f, Marshal a)
           => Ptr Internal.Relation -> f a -> IO ()
writeBytes relation fa = allocaBytes totalByteCount $ \ptr -> do
  runMarshalM (traverse_ push fa) ptr
  Internal.pushFacts relation ptr (fromIntegral objCount)
  where
    totalByteCount = getSum $ foldMap (Sum . numBytes) fa
    objCount = length fa
{-# INLINABLE writeBytes #-}

writeAsBytes :: (S.Storable a, Marshal a) => a -> CMarshal ()
writeAsBytes a = do
  ptr <- gets castPtr
  liftIO $ S.poke ptr a
  put $ ptr `plusPtr` numBytes a
{-# INLINABLE writeAsBytes #-}

readAsBytes :: (S.Storable a, Marshal a) => CMarshal a
readAsBytes = do
  ptr <- gets castPtr
  a <- liftIO $ S.peek ptr
  put $ ptr `plusPtr` numBytes a
  pure a
{-# INLINABLE readAsBytes #-}

