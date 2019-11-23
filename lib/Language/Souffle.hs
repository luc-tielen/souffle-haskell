
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Language.Souffle
  ( Program(..)
  , Souffle
  , Fact(..)
  , Marshal.Marshal(..)
  , init
  , run
  , loadFiles
  , writeFiles
  , addFact
  , addFacts
  , getFacts
  ) where

import Prelude hiding ( init )
import Data.Foldable ( traverse_ )
import Control.Monad.IO.Class
import Foreign.ForeignPtr
import Foreign.Ptr
import GHC.TypeLits
import Data.Proxy
import Data.Kind
import qualified Language.Souffle.Internal as Internal
import qualified Language.Souffle.Marshal as Marshal

-- TODO import Language.Souffle.Monad here, and dont use IO directly


newtype Souffle prog = Souffle (ForeignPtr Internal.Souffle)

class Program a where
  type ProgramFacts a :: [Type]
  programName :: Proxy a -> String

class Marshal.Marshal a => Fact a where
  factName :: Proxy a -> String

type family ContainsFact prog fact :: Constraint where
  ContainsFact prog fact =
    CheckContains prog (ProgramFacts prog) fact

type family CheckContains prog facts fact :: Constraint where
  CheckContains prog '[] fact =
    -- TODO list out all facts?
    (TypeError ('Text "Program of type '" ':<>: 'ShowType prog
          ':<>: 'Text "' does not contain fact: '"
          ':<>: 'ShowType fact ':<>: 'Text "'"))
  CheckContains _ (a ': _) a = ()
  CheckContains prog (_ ': as) b = CheckContains prog as b


init :: forall prog m. (Program prog, MonadIO m)
     => prog -> m (Maybe (Souffle prog))
init _ =
  let progName = programName (Proxy :: Proxy prog)
   in liftIO $ fmap Souffle <$> Internal.init progName

run :: MonadIO m => Souffle prog -> m ()
run (Souffle prog) = liftIO $ Internal.run prog

loadFiles :: MonadIO m => Souffle prog -> String -> m ()
loadFiles (Souffle prog) = liftIO . Internal.loadAll prog

writeFiles :: MonadIO m => Souffle prog -> m ()
writeFiles (Souffle prog) = liftIO $ Internal.printAll prog

getFacts :: forall a prog m. (Fact a, ContainsFact prog a, MonadIO m)
         => Souffle prog -> m [a]
getFacts (Souffle prog) = liftIO $ do
  let relationName = factName (Proxy :: Proxy a)
  relation <- Internal.getRelation prog relationName
  Internal.getRelationIterator relation >>= go []
  where
    go acc it = do
      hasNext <- Internal.relationIteratorHasNext it
      if hasNext
        then do
          tuple <- Internal.relationIteratorNext it
          result <- Marshal.runMarshalT Marshal.pop tuple
          go (result : acc) it
        else pure acc

addFact :: forall a prog m. (Fact a, ContainsFact prog a, MonadIO m)
        => Souffle prog -> a -> m ()
addFact (Souffle prog) fact = liftIO $ do
  let relationName = factName (Proxy :: Proxy a)
  relation <- Internal.getRelation prog relationName
  addFact' relation fact

addFacts :: forall a prog m. (Fact a, ContainsFact prog a, MonadIO m)
         => Souffle prog -> [a] -> m ()
addFacts (Souffle prog) facts = liftIO $ do
  let relationName = factName (Proxy :: Proxy a)
  relation <- Internal.getRelation prog relationName
  traverse_ (addFact' relation) facts

addFact' :: Fact a => Ptr Internal.Relation -> a -> IO ()
addFact' relation fact = do
  tuple <- Internal.allocTuple relation
  withForeignPtr tuple $ Marshal.runMarshalT (Marshal.push fact)
  Internal.addTuple relation tuple

