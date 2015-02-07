{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tree.Monadic (
   MTree(..),
   MNode(..),
   Path(..),
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

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Monad as C
import Control.Monad.Loops (whileJust)
import Control.Monad.STM
import Data.Functor.FunctorM
import Data.Functor.Monadic
import Data.Tree as T

import Debug.Trace

-- |A monadic rose tree in which the child nodes are wrapped in a monad.
--  @l@ is the type of the keys in the leaf nodes and @n@ the type
--  of the keys in the inner nodes.
data MTree m n = -- |An internal nodes with a value and children
                 --  wrapped in a monad.
                 MTree (m (MNode m n))

-- |A node of an 'MTree'.
data MNode m n = MNode {nodeContent::n, nodeChildren::[MTree m n]}

-- |A path in a tree.
type Path n = [n]

instance (Functor m) => Functor (MTree m) where
   fmap f (MTree m) = MTree $ fmap (fmap f) m

instance Functor m => Functor (MNode m) where
   fmap f (MNode n ns) = MNode (f n) (fmap (fmap f) ns)

instance (Functor m, Monad m) => FunctorM (MTree m) m where
   fmapM f (MTree m) = return $ MTree $ m >>= fmapM f

instance (Functor m, Monad m) => FunctorM (MNode m) m where
   fmapM f (MNode n ns) = liftM2 MNode (f n) (mapM (fmapM f) ns)

-- |Completely unrolls an 'MTree' into a 'Tree' __depth-first__,
--  evaluating all nodes.
--
--  The time is @O(n)@ where @n@ is the number of nodes.
--  The space is @O(n)@ if the result is kept and @O(d)@ if it isn't,
--  with @d@ being the maximal depth of the tree.
materialize :: Monad m => MTree m n -> m (Tree n)
materialize (MTree m) = do
   (MNode v children) <- m
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
materializePar :: Int
                  -- ^The upper limit on simultaneous tasks.
                  --  For @n=1@, 'materializePar' behaves identically to
                  --  materialize. For very large @n@, every node gets its own
                  --  thread. Depending on the IO operations, this value should
                  --  be kept within reason.
               -> MTree IO n
               -> IO (Tree n)
materializePar n tree = do numTasks <- atomically $ newTVar n
                           materializePar' numTasks tree
   where
      -- we have three TVars:
      -- first, the global number of concurrently active tasks (numTasks).
      -- then, for each node, a queue resQ and a counter childRes
      -- childRes indicates how many children of a node have yet to finish
      -- processing and resQ stores their results. We block until
      -- all our children have finished. and then we get all elements from
      -- resQ.
      -- Note that numTasks blocks IF IT REACHES 0, while
      -- childRes blocks UNTIL it reaches 0.

      materializePar' numTasks (MTree m) = do
         -- the processing of our monadic value counts as a task
         atomically $ addTask numTasks
         (MNode v children) <- m
         atomically $ removeTask numTasks

         -- we start processing our children

         -- TVars for the children
         resQ <- atomically $ newTQueue
         childRes <- atomically $ newTVar $ length children
         -- start recursive processing
         mapM (f resQ numTasks childRes) children
         -- block until all children are finished
         status <- atomically $ readTVar childRes
         atomically (readTVar childRes >>= check . (<=0))
         -- collect the results and return
         results <- atomically $ readWholeQueue resQ
         return $ Node v results


      -- |Starts a new instance of materializePar'.
      f resQueue numTasks childRes node =
         forkIO (do nodeRes <- materializePar' numTasks node
                    atomically (writeTQueue resQueue nodeRes
                                >> modifyTVar childRes (subtract 1)))

      -- |Reads all elements of a queue.
      readWholeQueue :: TQueue a -> STM [a]
      readWholeQueue q = whileJust (tryReadTQueue q) return

      -- |Decreases a value by 1 and blocks if the result would be below 0.
      --  A semaphore's "wait".
      addTask :: TVar Int -> STM ()
      addTask v = do v' <- readTVar v
                     if v' <= 0 then retry else writeTVar v (v'-1)

      -- |Increases a value by one. A semaphore's "signal"-
      removeTask :: TVar Int -> STM ()
      removeTask = flip modifyTVar' (+1)


-- |Unfolds an 'MTree' from a monadic value.
--  Analogous to 'Data.Tree.unfoldTreeM'
unfoldMTree :: Monad m => (b -> m (a, [b])) -> m b -> MTree m a
unfoldMTree f x = MTree $ do (y, ys) <- x >>= f
                             return $ MNode y $ map (unfoldMTree f . return) ys

-- |Leaf function on trees.
leaves :: (s -> n -> a) -- ^Result calculator, applied to the leaves.
       -> (s -> n -> s) -- ^State updater, applied on non-leaves.
       -> s -- ^Initial state.
       -> T.Tree n
       -> [a]
leaves f g seed (Node n []) = [f seed n]
leaves f g seed (Node n xs) = concatMap (leaves f g (g seed n)) xs

traverseM :: Monad m
          => (Bool -> s -> n -> m (s,a))
          -> s
          -> MTree m n
          -> MTree m a
traverseM f st (MTree m) = MTree $ do
   (MNode n ns) <- m
   (st',n') <- f (null ns) st n
   return $ MNode n' $ map (traverseM f st') ns



-- |Collects just the leaves of a tree. Convenience function.
justLeaves :: (n -> a) -> T.Tree n -> [a]
justLeaves f = leaves (const . f) (const id) undefined
