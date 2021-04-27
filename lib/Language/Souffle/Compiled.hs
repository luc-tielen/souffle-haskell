{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies, DerivingVia #-}
{-# LANGUAGE BangPatterns, RoleAnnotations, MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs, DataKinds, TypeApplications, TypeOperators #-}
{-# LANGUAGE ConstraintKinds, PolyKinds #-}

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
  , Submit
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
import qualified Data.Array as A
import qualified Data.Array.IO as A
import qualified Data.Array.Unsafe as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Int
import Data.Word
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign (copyBytes)
import Foreign.Ptr
import qualified Foreign.Storable as S
import GHC.Generics
import Language.Souffle.Class
import qualified Language.Souffle.Internal as Internal
import Language.Souffle.Marshal
import Control.Concurrent


type ByteCount = Int
type ByteBuf = Internal.ByteBuf

-- TODO: rename later
data BufData
  = BufData
  { bufPtr :: {-# UNPACK #-} !(ForeignPtr ByteBuf)
  , bufSize :: {-# UNPACK #-} !ByteCount
  }

-- | A datatype representing a handle to a datalog program.
--   The type parameter is used for keeping track of which program
--   type the handle belongs to for additional type safety.
data Handle prog
  = Handle {-# UNPACK #-} !(ForeignPtr Internal.Souffle)
           (MVar BufData)
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
        souffleHandle <- liftIO $ Internal.init progName
        case souffleHandle of
          Nothing -> action Nothing
          Just souffleHandle' -> do
            --TODO is this ok? no finalizer?
            bufData <- liftIO $ do
              ptr <- newForeignPtr_ nullPtr
              newMVar $ BufData ptr 0
            action $ Just $ Handle souffleHandle' bufData
   in result

-- | A monad used solely for marshalling and unmarshalling
--   between Haskell and Souffle Datalog. This fast variant is used when the
--   marshalling from Haskell to C++ and the exact size of a datastructure
--   is statically known (read: data type contains no string-like types),
--   or when marshalling from C++ to Haskell (pointer is then managed by C++).
newtype CMarshalFast a = CMarshalFast (StateT (Ptr ByteBuf) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (Ptr ByteBuf))
  via (StateT (Ptr ByteBuf) IO)

runMarshalFastM :: CMarshalFast a -> Ptr ByteBuf -> IO a
runMarshalFastM (CMarshalFast m) = evalStateT m
{-# INLINABLE runMarshalFastM #-}

-- NOTE: assumes Souffle is compiled with 32-bit RAM domain.
ramDomainSize :: Int
ramDomainSize = 4

writeAsBytes :: (S.Storable a, Marshal a) => a -> CMarshalFast ()
writeAsBytes a = do
  ptr <- gets castPtr
  liftIO $ S.poke ptr a
  put $ ptr `plusPtr` ramDomainSize
{-# INLINABLE writeAsBytes #-}

readAsBytes :: (S.Storable a, Marshal a) => CMarshalFast a
readAsBytes = do
  ptr <- gets castPtr
  a <- liftIO $ S.peek ptr
  put $ ptr `plusPtr` ramDomainSize
  pure a
{-# INLINABLE readAsBytes #-}

instance MonadPush CMarshalFast where
  pushInt32 = writeAsBytes
  {-# INLINABLE pushInt32 #-}
  pushUInt32 = writeAsBytes
  {-# INLINABLE pushUInt32 #-}
  pushFloat = writeAsBytes
  {-# INLINABLE pushFloat #-}
  pushString str = pushText $ TS.pack str
  {-# INLINABLE pushString #-}
  pushTextUtf16 str = pushText $ TS.fromText str
  {-# INLINABLE pushTextUtf16 #-}
  pushText _ =
    error "Fast marshalling does not support serializing string-like values."
  {-# INLINABLE pushText #-}

instance MonadPop CMarshalFast where
  popInt32 = readAsBytes
  {-# INLINABLE popInt32 #-}
  popUInt32 = readAsBytes
  {-# INLINABLE popUInt32 #-}
  popFloat = readAsBytes
  {-# INLINABLE popFloat #-}
  popString = TS.unpack <$> popText
  {-# INLINABLE popString #-}
  popTextUtf16 = TS.toText <$> popText
  {-# INLINABLE popTextUtf16 #-}
  popText = do
    byteCount <- popUInt32
    if byteCount == 0
      then pure TS.empty
      else do
        ptr <- gets castPtr
        bs <- liftIO $ BSU.unsafePackCStringLen (ptr, fromIntegral byteCount)
        put $ ptr `plusPtr` fromIntegral byteCount
        -- NOTE: $! is needed here to force the text value. A copy needs to
        -- be made (using toShort), before the bytearray is overwritten.
        pure $! TSU.fromShortByteStringUnsafe $ BSS.toShort bs
  {-# INLINABLE popText #-}


data MarshalState
  = MarshalState
  { _buf :: {-# UNPACK #-} !BufData
  , _ptr :: {-# UNPACK #-} !(Ptr ByteBuf)
  , _ptrOffset :: {-# UNPACK #-} !Int
  }

-- | A monad used solely for marshalling from Haskell to Souffle Datalog (C++).
--   This slow variant is used when the exact size of a datastructure is *not*
--   statically known (read: data type contains string-like types).
newtype CMarshalSlow a = CMarshalSlow (StateT MarshalState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState MarshalState)
  via (StateT MarshalState IO)

runMarshalSlowM :: BufData -> Int -> CMarshalSlow a -> IO a
runMarshalSlowM bufData byteCount (CMarshalSlow m) = do
  bufData' <- if bufSize bufData > byteCount
    then pure bufData
    else do
      byteArray <- allocateBuf byteCount
      pure $ BufData byteArray byteCount
  let ptr = unsafeForeignPtrToPtr (bufPtr bufData')
  evalStateT m $ MarshalState bufData' ptr 0
{-# INLINABLE runMarshalSlowM #-}

resizeBufWhenNeeded :: ByteCount -> CMarshalSlow ()
resizeBufWhenNeeded byteCount = do
  MarshalState bufData _ offset <- get
  let totalByteCount = bufSize bufData
  when (byteCount + offset > totalByteCount) $ do
    let newTotalByteCount = getNewTotalByteCount byteCount offset totalByteCount
    newBuf <- allocateBuf newTotalByteCount
    copyBuf newBuf (bufPtr bufData) totalByteCount
    let newPtr = unsafeForeignPtrToPtr newBuf
        bufData' = BufData newBuf newTotalByteCount
    put $ MarshalState bufData' (newPtr `plusPtr` offset) offset
{-# INLINABLE resizeBufWhenNeeded #-}

allocateBuf :: MonadIO m => ByteCount -> m (ForeignPtr ByteBuf)
allocateBuf byteCount = liftIO $
  mallocForeignPtrBytes byteCount
{-# INLINABLE allocateBuf #-}

copyBuf :: ForeignPtr ByteBuf -> ForeignPtr ByteBuf -> Int -> CMarshalSlow ()
copyBuf dst src byteCount = liftIO $
  withForeignPtr src $ \srcPtr ->
  withForeignPtr dst $ \dstPtr ->
    copyBytes dstPtr srcPtr byteCount
{-# INLINABLE copyBuf #-}

getNewTotalByteCount :: ByteCount -> Int -> ByteCount -> ByteCount
getNewTotalByteCount byteCount offset = go where
  go totalByteCount
    | byteCount + offset > totalByteCount = go (totalByteCount * 2)
    | otherwise = totalByteCount
{-# INLINABLE getNewTotalByteCount #-}

incrementPtr :: ByteCount -> CMarshalSlow ()
incrementPtr byteCount =
  modify $ \(MarshalState buf ptr offset) ->
    MarshalState buf (ptr `plusPtr` byteCount) (offset + byteCount)
{-# INLINABLE incrementPtr #-}

instance MonadPush CMarshalSlow where
  pushInt32 = writeAsBytesSlow
  {-# INLINABLE pushInt32 #-}
  pushUInt32 = writeAsBytesSlow
  {-# INLINABLE pushUInt32 #-}
  pushFloat = writeAsBytesSlow
  {-# INLINABLE pushFloat #-}
  pushString str = pushText $ TS.pack str
  {-# INLINABLE pushString #-}
  pushTextUtf16 str = pushText $ TS.fromText str
  {-# INLINABLE pushTextUtf16 #-}
  pushText txt = do
    let bs = TS.toByteString txt  -- TODO: is it possible to get rid of this copy?
        len = BS.length bs
    resizeBufWhenNeeded (ramDomainSize + len)
    pushUInt32 (fromIntegral len)
    if len == 0
      then pure ()
      else do
        ptr <- gets (castPtr . _ptr)
        liftIO $ BSU.unsafeUseAsCString bs $ flip (copyBytes ptr) len
        incrementPtr len
  {-# INLINABLE pushText #-}

writeAsBytesSlow :: (S.Storable a, Marshal a) => a -> CMarshalSlow ()
writeAsBytesSlow a = do
  resizeBufWhenNeeded ramDomainSize
  ptr <- gets (castPtr . _ptr)
  liftIO $ S.poke ptr a
  incrementPtr ramDomainSize
{-# INLINABLE writeAsBytesSlow #-}


class Collect c where
  collect :: Marshal a => Word32 -> CMarshalFast (c a)

instance Collect [] where
  collect objCount = go objCount [] where
    go count acc
      | count == 0 = pure acc
      | otherwise = do
        !x <- pop
        go (count - 1) (x:acc)
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
      collect' :: Marshal a => A.IOArray Int a -> Int -> CMarshalFast (A.Array Int a)
      collect' array idx
        | idx == objCount' = liftIO $ A.unsafeFreeze array
        | otherwise = do
          !obj <- pop
          liftIO $ A.writeArray array idx obj
          collect' array (idx + 1)
  {-# INLINABLE collect #-}

-- | A helper typeclass constraint, needed to serialize Datalog facts from
--   Haskell to C++.
type Submit a = ToByteSize (Rep a)

instance MonadSouffle SouffleM where
  type Handler SouffleM = Handle
  type CollectFacts SouffleM c = Collect c
  type SubmitFacts SouffleM a = Submit a

  run (Handle prog _) = SouffleM $ Internal.run prog
  {-# INLINABLE run #-}

  setNumThreads (Handle prog _) numCores =
    SouffleM $ Internal.setNumThreads prog numCores
  {-# INLINABLE setNumThreads #-}

  getNumThreads (Handle prog _) =
    SouffleM $ Internal.getNumThreads prog
  {-# INLINABLE getNumThreads #-}

  addFact :: forall a prog. (Fact a, ContainsInputFact prog a, Submit a)
          => Handle prog -> a -> SouffleM ()
  addFact (Handle prog bufVar) fact = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    writeBytes bufVar relation (Identity fact)
  {-# INLINABLE addFact #-}

  addFacts :: forall t a prog. (Foldable t, Fact a, ContainsInputFact prog a, Submit a)
           => Handle prog -> t a -> SouffleM ()
  addFacts (Handle prog bufVar) facts = liftIO $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    writeBytes bufVar relation facts
  {-# INLINABLE addFacts #-}

  getFacts :: forall a c prog. (Fact a, ContainsOutputFact prog a, Collect c)
           => Handle prog -> SouffleM (c a)
  getFacts (Handle prog _) = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    buf <- withForeignPtr prog $ flip Internal.popFacts relation
    flip runMarshalFastM buf $ collect =<< popUInt32
  {-# INLINABLE getFacts #-}

  findFact :: forall a prog. (Fact a, ContainsOutputFact prog a, Submit a)
           => Handle prog -> a -> SouffleM (Maybe a)
  findFact (Handle prog bufVar) fact = SouffleM $ do
    let relationName = factName (Proxy :: Proxy a)
    relation <- Internal.getRelation prog relationName
    found <- case estimateNumBytes (Proxy @a) of
      Exact numBytes -> do
        modifyMVarMasked bufVar $ \bufData -> do
          bufData' <- if bufSize bufData > numBytes
            then pure bufData
            else do
              byteArray <- allocateBuf numBytes
              pure $ BufData byteArray numBytes
          found <- withForeignPtr (bufPtr bufData') $ \ptr -> do
            runMarshalFastM (push fact) ptr
            Internal.containsFact relation ptr
          pure (bufData', found)
      Estimated numBytes -> modifyMVarMasked bufVar $ \bufData ->
        runMarshalSlowM bufData numBytes $ do
          push fact
          bufData' <- gets _buf
          liftIO $ withForeignPtr (bufPtr bufData') $ \ptr -> do
            found <- Internal.containsFact relation ptr
            pure (bufData', found)
    pure $ if found then Just fact else Nothing
  {-# INLINABLE findFact #-}

instance MonadSouffleFileIO SouffleM where
  loadFiles (Handle prog _) = SouffleM . Internal.loadAll prog
  {-# INLINABLE loadFiles #-}

  writeFiles (Handle prog _) = SouffleM . Internal.printAll prog
  {-# INLINABLE writeFiles #-}


data ByteSize
  = Exact {-# UNPACK #-} !ByteCount
  | Estimated {-# UNPACK #-} !ByteCount

instance Semigroup ByteSize where
  Exact s1 <> Exact s2 = Exact (s1 + s2)
  Exact s1 <> Estimated s2 = Estimated (s1 + s2)
  Estimated s1 <> Exact s2 = Estimated (s1 + s2)
  Estimated s1 <> Estimated s2 = Estimated (s1 + s2)
  {-# INLINABLE (<>) #-}

class ToByteSize (a :: k) where
  toByteSize :: Proxy a -> ByteSize

instance ToByteSize Int32 where
  toByteSize = const $ Exact 4
  {-# INLINABLE toByteSize #-}

instance ToByteSize Word32 where
  toByteSize = const $ Exact 4
  {-# INLINABLE toByteSize #-}

instance ToByteSize Float where
  toByteSize = const $ Exact 4
  {-# INLINABLE toByteSize #-}

instance ToByteSize String where
  -- 4 for length prefix + 32 for actual string
  toByteSize = const $ Estimated 36
  {-# INLINABLE toByteSize #-}

instance ToByteSize T.Text where
  -- 4 for length prefix + 32 for actual string
  toByteSize = const $ Estimated 36
  {-# INLINABLE toByteSize #-}

instance ToByteSize TL.Text where
  -- 4 for length prefix + 32 for actual string
  toByteSize = const $ Estimated 36
  {-# INLINABLE toByteSize #-}

instance ToByteSize TS.ShortText where
  -- 4 for length prefix + 32 for actual string
  toByteSize = const $ Estimated 36
  {-# INLINABLE toByteSize #-}

instance ToByteSize a => ToByteSize (K1 i a) where
  toByteSize = const $ toByteSize (Proxy @a)
  {-# INLINABLE toByteSize #-}

instance ToByteSize a => ToByteSize (M1 i c a) where
  toByteSize = const $ toByteSize (Proxy @a)
  {-# INLINABLE toByteSize #-}

instance (ToByteSize f, ToByteSize g) => ToByteSize (f :*: g) where
  toByteSize = const $
    toByteSize (Proxy @f) <> toByteSize (Proxy @g)
  {-# INLINABLE toByteSize #-}

estimateNumBytes :: forall a. ToByteSize (Rep a) => Proxy a -> ByteSize
estimateNumBytes _ = toByteSize (Proxy @(Rep a))
{-# INLINABLE estimateNumBytes #-}

writeBytes :: forall f a. (Foldable f, Marshal a, ToByteSize (Rep a))
           => MVar BufData -> Ptr Internal.Relation -> f a -> IO ()
writeBytes bufVar relation fa = case estimateNumBytes (Proxy @a) of
  Exact numBytes -> modifyMVarMasked_ bufVar $ \bufData -> do
    let totalByteCount = numBytes * objCount
    bufData' <- if bufSize bufData > totalByteCount
      then pure bufData
      else do
        byteArray <- allocateBuf totalByteCount
        pure $ BufData byteArray totalByteCount
    withForeignPtr (bufPtr bufData') $ \ptr -> do
      runMarshalFastM (traverse_ push fa) ptr
      Internal.pushFacts relation ptr (fromIntegral objCount)
    pure bufData'

  Estimated numBytes -> modifyMVarMasked_ bufVar $ \bufData ->
    runMarshalSlowM bufData (numBytes * objCount) $ do
      traverse_ push fa
      bufData' <- gets _buf
      liftIO $ withForeignPtr (bufPtr bufData') $ \ptr -> do
        Internal.pushFacts relation ptr (fromIntegral objCount)
        pure bufData'
  where objCount = length fa
{-# INLINABLE writeBytes #-}

