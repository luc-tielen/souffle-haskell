{-# LANGUAGE UndecidableInstances, TupleSections #-}

module Language.Souffle.Analysis
  ( Analysis
  , mkAnalysis
  , execAnalysis
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Control.Arrow
import Data.Profunctor

-- NOTE: 2nd a is mostly used for Category/Arrow instances
data Analysis m a b
  = Analysis (a -> m ()) (m ()) (a -> m b)

mkAnalysis :: (a -> m ()) -> m () -> m b -> Analysis m a b
mkAnalysis f r g = Analysis f r (const g)

execAnalysis :: Applicative m => Analysis m a b -> (a -> m b)
execAnalysis (Analysis f r g) a = f a *> r *> g a

instance Functor m => Functor (Analysis m a) where
  fmap func (Analysis f r g) =
    Analysis f r (fmap func <$> g)

instance Functor m => Profunctor (Analysis m) where
  lmap fn (Analysis f r g) =
    Analysis (lmap fn f) r (lmap fn g)
  rmap = fmap

instance (Monoid (m ()), Applicative m) => Applicative (Analysis m a) where
  pure a = Analysis mempty mempty (const $ pure a)

  Analysis f1 r1 g1 <*> Analysis f2 r2 g2 =
    Analysis (f1 <> f2) (r1 <> r2) (\a -> g1 a <*> g2 a)

instance (Semigroup (m ()), Semigroup (m b)) => Semigroup (Analysis m a b) where
  Analysis f1 r1 g1 <> Analysis f2 r2 g2 =
    Analysis (f1 <> f2) (r1 <> r2) (g1 <> g2)

instance (Monoid (m ()), Monoid (m b)) => Monoid (Analysis m a b) where
  mempty = Analysis mempty mempty mempty

instance (Monoid (m ()), Monad m) => Category (Analysis m) where
  id = Analysis mempty mempty pure

  Analysis f1 r1 g1 . Analysis f2 r2 g2 = Analysis f r1 g
    where
      f = execAnalysis (Analysis f2 r2 g2) >=> f1
      -- NOTE: lazyness avoids work here in g2 in cases where "const" is used
      g = g2 >=> g1

instance Functor m => Strong (Analysis m) where
  first' (Analysis f r g) =
    Analysis (f . fst) r $ \(b, d) -> (,d) <$> g b

  second' (Analysis f r g) =
    Analysis (f . snd) r $ \(d, b) -> (d,) <$> g b

instance Applicative m => Choice (Analysis m) where
  left' (Analysis f r g) = Analysis f' r g'
    where
      f' = \case
        Left b -> f b
        Right _ -> pure ()
      g' = \case
        Left b -> Left <$> g b
        Right d -> pure $ Right d

  right' (Analysis f r g) = Analysis f' r g'
    where
      f' = \case
        Left _ -> pure ()
        Right b -> f b
      g' = \case
        Left d -> pure $ Left d
        Right b -> Right <$> g b

instance (Monad m, Monoid (m ()), Category (Analysis m)) => Arrow (Analysis m) where
  arr f = Analysis mempty mempty (pure . f)

  first = first'

  second = second'

  Analysis f1 r1 g1 *** Analysis f2 r2 g2 =
    Analysis (\(b, b') -> f1 b *> f2 b') (r1 <> r2) $ \(b, b') -> do
      c <- g1 b
      c' <- g2 b'
      pure (c, c')

  Analysis f1 r1 g1 &&& Analysis f2 r2 g2 =
    Analysis (f1 <> f2) (r1 <> r2) $ \b -> (,) <$> g1 b <*> g2 b

instance (Monad m, Monoid (m ())) => ArrowChoice (Analysis m) where
  left = left'

  right = right'
