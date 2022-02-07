module Language.Souffle.Analysis
  ( Analysis(..)
  , execAnalysis
  ) where

import Data.Profunctor

data Analysis m a b
  = Analysis (a -> m ()) (m ()) (m b)

execAnalysis :: Applicative m
             => Analysis m a b
             -> (a -> m b)
execAnalysis (Analysis f r g) a = f a *> r *> g

instance Functor m => Functor (Analysis m a) where
  fmap func (Analysis f r g) =
    Analysis f r (func <$> g)

instance Functor m => Profunctor (Analysis m) where
  lmap left (Analysis f r g) =
    Analysis (lmap left f) r g
  rmap = fmap

instance (Monoid (m ()), Applicative m) => Applicative (Analysis m a) where
  pure a = Analysis mempty mempty (pure a)

  Analysis f1 r1 g1 <*> Analysis f2 r2 g2 =
    Analysis (f1 <> f2) (r1 <> r2) (g1 <*> g2)

instance (Semigroup (m ()), Semigroup (m b)) => Semigroup (Analysis m a b) where
  Analysis f1 r1 g1 <> Analysis f2 r2 g2 =
    Analysis (f1 <> f2) (r1 <> r2) (g1 <> g2)

instance (Monoid (m ()), Monoid (m b)) => Monoid (Analysis m a b) where
  mempty = Analysis mempty mempty mempty
