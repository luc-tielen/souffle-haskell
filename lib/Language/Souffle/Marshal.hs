
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE DerivingVia, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE UndecidableInstances, DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators, RankNTypes #-}

-- | This module exposes a uniform interface to marshal values
--   to and from Souffle Datalog. This is done via the 'Marshal' typeclass
--   and 'MarshalT' monad transformer.
--   Also, a mechanism is exposed for generically deriving marshalling
--   and unmarshalling code for simple product types.
module Language.Souffle.Marshal
  ( Marshal(..)
  , MarshalF(..)
  , MarshalM
  , interpret
  ) where

import Control.Monad.Free
import GHC.Generics
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Language.Souffle.Internal.Constraints as C



data MarshalF a
  = PopInt  (Int32  -> a)
  | PopStr  (String -> a)
  | PushInt Int32  a
  | PushStr String a

instance Functor MarshalF where
  fmap f m = case m of
    PopInt g -> PopInt (f . g)
    PopStr g -> PopStr (f . g)
    PushInt i x -> PushInt i (f x)
    PushStr s x -> PushStr s (f x)

type MarshalM a = Free MarshalF a

cmd :: Functor f => f a -> Free f a
cmd command = Free (fmap Pure command)

interpret :: Monad m => (forall x . MarshalF x -> m x) -> MarshalM a -> m a
interpret = foldFree

{- | A typeclass for providing a uniform API to marshal/unmarshal values
     between Haskell and Souffle datalog.

The marshalling is done via a stack-based approach, where elements are
pushed/popped one by one. You need to make sure that the marshalling
of values happens in the correct order or unexpected things might happen
(including crashes). Pushing and popping of fields should happen in the
same order (from left to right, as defined in Datalog).

Generic implementations for 'push' and 'pop' that perform the previously
described behavior are available. This makes it possible to
write very succinct code:

@
data Edge = Edge String String deriving Generic

instance Marshal Edge
@
-}
class Marshal a where
  -- | Marshals a value to the datalog side.
  push :: a -> MarshalM ()
  -- | Unmarshals a value from the datalog side.
  pop :: MarshalM a

  default push :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a))
               => a -> MarshalM ()
  default pop :: (Generic a, C.SimpleProduct a (Rep a), GMarshal (Rep a))
              => MarshalM a
  push a = gpush (from a)
  {-# INLINABLE push #-}
  pop = to <$> gpop
  {-# INLINABLE pop #-}

instance Marshal Int32 where
  push int = cmd (PushInt int ())
  {-# INLINABLE push #-}
  pop  = cmd (PopInt id)
  {-# INLINABLE pop #-}

instance Marshal String where
  push str = cmd (PushStr str ())
  {-# INLINABLE push #-}
  pop  = cmd (PopStr id)
  {-# INLINABLE pop #-}

instance Marshal T.Text where
  push = push . T.unpack
  {-# INLINABLE push #-}
  pop  = T.pack <$> pop
  {-# INLINABLE pop #-}

instance Marshal TL.Text where
  push = push . TL.unpack
  {-# INLINABLE push #-}
  pop  = TL.pack <$> pop
  {-# INLINABLE pop #-}

class GMarshal f where
  gpush :: f a -> MarshalM ()
  gpop  :: MarshalM (f a)

instance Marshal a => GMarshal (K1 i a) where
  gpush (K1 x) = push x
  {-# INLINABLE gpush #-}
  gpop = K1 <$> pop
  {-# INLINABLE gpop #-}

instance (GMarshal f, GMarshal g) => GMarshal (f :*: g) where
  gpush (a :*: b) = do
    gpush a
    gpush b
  {-# INLINABLE gpush #-}
  gpop = (:*:) <$> gpop <*> gpop
  {-# INLINABLE gpop #-}

instance GMarshal a => GMarshal (M1 i c a) where
  gpush (M1 x) = gpush x
  {-# INLINABLE gpush #-}
  gpop = M1 <$> gpop
  {-# INLINABLE gpop #-}
