{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tree.Monadic (
   MTree(..),
   -- * Running trees
   --   These functions traverse MTrees and return pure rose trees.
   --   Their drawback is that they keep the entire tree in memory.
   --   If you are only interested in the monadic effects and not
   --   the actual values, it is best to discard the result to save space.
   materialize,
   materializePar,
   -- * Folding, unfolding MTrees
   unfoldMTree,
   leaves,
   justLeaves,
   traverseM,
   ) where

import Control.Concurrent.STM.Utils
import Control.Monad as C
import Data.Collections.Instances()
import Data.Functor.FunctorM
import Data.Tree as T

-- |A monadic rose tree in which the child nodes are wrapped in a monad.
--  @l@ is the type of the keys in the leaf nodes and @n@ the type
--  of the keys in the inner nodes.
data MTree m n = -- |An internal nodes with a value and children
                 --  wrapped in a monad.
                 MTree (m (n, [MTree m n]))

instance (Functor m) => Functor (MTree m) where
   fmap f (MTree m) = MTree $ fmap (\(x,xs) -> (f x, map (fmap f) xs)) m

instance (Functor m, Monad m) => FunctorM (MTree m) m where
   fmapM f (MTree m) = return $ MTree $ do
      (x,xs) <- m
      liftM2 (,) (f x) (mapM (fmapM f) xs)

-- |Completely unrolls an 'MTree' into a 'Tree' __depth-first__,
--  evaluating all nodes.
--
--  The time is @O(n)@ where @n@ is the number of nodes.
--  The space is @O(n)@ if the result is kept and @O(d)@ if it isn't,
--  with @d@ being the maximal depth of the tree.
materialize :: Monad m => MTree m n -> m (Tree n)
materialize (MTree m) = do
   (v, children) <- m
   children' <- mapM materialize children
   return $ T.Node v children'

-- |Unrolls an 'MTree' into a tree, executing the monadic operations in
--  parallel. 'materializePar' is generalization of 'materialize' and is
--  superior to it if the 'MTree' contains IO-heavy
--  operations like HTTP requests.
--
--  The time @Omega(n/t)@, where @n@ is the number of nodes and @t@ is the
--  size of the thread pool - with optimal parallelism, @t@ nodes can be
--  processed at once (although this depends on the shape of the tree. In
--  the worst case of a list, @t@ makes no difference at all.
--  The space is @O(n)@ if the result is kept and @O(dt)@ if it isn't.
--
--  Note that a node's children may be rearranged, depending
--  on the order in which their processing finishes.
materializePar :: TaskLimit
                  -- ^The upper limit on simultaneous tasks.
                  --  For @n=1@, 'materializePar' behaves identically to
                  --  materialize. For very large @n@, every node gets its own
                  --  thread. Depending on the IO operations, this value should
                  --  be kept within reason.
               -> MTree IO n
               -> IO (Tree n)
materializePar numTasks (MTree m) = do
   (v, children) <- withTaskLimit numTasks m
   results <- parMapSTM (materializePar numTasks) children
   return $ Node v results

-- |Unfolds an 'MTree' from a monadic value.
--  Analogous to 'Data.Tree.unfoldTreeM'
unfoldMTree :: Monad m => (b -> m (a, [b])) -> m b -> MTree m a
unfoldMTree f x = MTree $ do (y, ys) <- x >>= f
                             return $ (y, map (unfoldMTree f . return) ys)

-- |Leaf function on trees.
leaves :: (s -> n -> a) -- ^Result calculator, applied to the leaves.
       -> (s -> n -> s) -- ^State updater, applied on non-leaves.
       -> s -- ^Initial state.
       -> T.Tree n
       -> [a]
leaves f _ seed (Node n []) = [f seed n]
leaves f g seed (Node n xs) = concatMap (leaves f g (g seed n)) xs

traverseM :: Monad m
          => (s -> n -> m (s,a))
          -> s
          -> MTree m n
          -> MTree m a
traverseM f st (MTree m) = MTree $ do
   (n, ns) <- m
   (st',n') <- f st n
   return $ (n', map (traverseM f st') ns)

-- |Collects just the leaves of a tree. Convenience function.
justLeaves :: (n -> a) -> T.Tree n -> [a]
justLeaves f = leaves (const f) undefined undefined
