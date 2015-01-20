{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |The class of data structures which support a monadic fmap, i.e.
--  a generalization of 'Control.Monad.mapM'.
--
--  Whereas 'Control.Monad.mapM' has type @(a -> m b) -> [a] -> m [b]@,
--  'Data.Functor.FunctorM.fmapM' has type @(a -> m b) -> f a -> m (f b)@.
--
--  'FunctorM' might look like a copy of 'Data.Traversable', but it's more
--  specific because 'fmapM' restricts the used functor, whereas 'traverse' does
--  not. 'FunctorM' and 'MonpTraversable' are orthogonal and analogous
--  specialisations of 'Traversable': 'MonoTraversable' only allows mapping to
--  a given data type (@Applicative f => (a -> f a) -> t a -> f (t a)@);
--  'FunctorM' only allows using a given functor (@(a -> f b) -> t a -> f (t b)@ -
--  note that @f@ is instance-level).
module Data.Functor.FunctorM where

import Control.Monad
import qualified Data.Tree as T

-- |Data structures to which we can transparently apply a monad function.
class (Functor f, Functor m) => FunctorM f m where
   -- |Maps every element of a structure to a monadic value and collects
   --  the results while preserving the original structure.
   fmapM :: (a -> m b) -> f a -> m (f b)
   -- |Maps every element of a structure to a monadic value and throws away
   --  the results.
   fmapM_ :: (a -> m b) -> f a -> m ()
   fmapM_ f = void . fmapM f

instance (Functor m, Monad m) => FunctorM [] m where
   fmapM = mapM
   fmapM_ = mapM_

instance (Functor m, Monad m) => FunctorM T.Tree m where
   fmapM f (T.Node v vs) = liftM2 T.Node (f v) (mapM (fmapM f) vs)
   fmapM_ f (T.Node v vs) = f v >> mapM_ (fmapM f) vs >> return ()
