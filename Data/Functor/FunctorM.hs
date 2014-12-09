-- |The class of data structures which support a monadic fmap, i.e.
--  a generalization of 'Control.Monad.mapM'.
--
--  Whereas 'Control.Monad.mapM' has type @(a -> m b) -> [a] -> m [b]@,
--  'Data.Functor.FunctorM.fmapM' has type @(a -> m b) -> f a -> m (f b)@.
module Data.Functor.FunctorM where

-- |Data structures to which we can transparently apply a monad function.
class Functor f => FunctorM f m where
   -- |Maps every element of a structure to a monadic value and collects
   --  the results while preserving the original structure.
   fmapM :: (a -> m b) -> f a -> m (f b)
   -- |Maps every element of a structure to a monadic value and throws away
   --  the results.
   fmapM_ :: (a -> m b) -> f a -> m ()

instance Monad m => FunctorM [] m where
   mapM = C.mapM
   mapM_ = C.mapM_

instance Functor f => FunctorM f Identity where
   mapM f = fmap (fromIdentity.f)
   mapM_ _ _ = return ()

instance Monad m => FunctorM T.Tree m where
   mapM f (T.Node v vs) = C.liftM2 T.Node (f v) (mapM f vs)
   mapM_ f (T.Node v vs) = f v >> mapM_ f vs >> return ()
