{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tree.Monadic where

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
   fmapM f (MTree m) = m >>= fmapM f >$> MTree . return

instance (Functor m, Monad m) => FunctorM (MNode m) m where
   fmapM f (MNode n ns) = liftM2 MNode (f n) (mapM (fmapM f) ns)

-- |Completely unrolls an 'MTree' into a 'Tree', evaluating all nodes.
materialize :: Monad m => MTree m n -> m (Tree n)
materialize (MTree m) = do
   (MNode v children) <- m
   children' <- mapM materialize children
   return $ T.Node v children'

-- |Unrolls an 'MTree' into a tree, executing the monadic operations in
--  parallel.
--
--  This is a generalization of 'materialize' and is much
--  superior if the 'MTree' contains IO-heavy operations like HTTP requests.
materializePar :: Show n => Int
                  -- ^The upper limit on simultaneous tasks.
                  --  A value of 1 results in an ordering of operations
                  --  identical to that of 'materialize'; a value of below
                  --  0 will cause non-termination.
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
         (MNode v children) <- m

         resQ <- atomically $ newTQueue
         childRes <- atomically $ newTVar $ length children

         mapM (f resQ numTasks childRes) children
         --traceM $ "waiting for children of " ++ show v ++ " to finish..."
         -- block until all children are finished
         status <- atomically $ readTVar childRes
         --traceM $ "childRes (" ++ show v ++ ")=" ++ show status
         atomically (readTVar childRes >>= check . (<=0))
         --traceM $ "collecting results of" ++ show v ++ "..."
         -- collect the results and return
         results <- atomically $ readWholeQueue resQ
         --traceM $ "done with " ++ show v
         return $ Node v results


      -- |Starts a new instance of materializePar'.
      --  Blocks as long as numTask <= 0.
      f resQueue numTasks childRes node =
         do atomically $ addTask numTasks
            forkIO (do nodeRes <- materializePar' numTasks node
                       atomically (writeTQueue resQueue nodeRes
                                   >> removeTask numTasks
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
leaves :: (n -> m -> a) -- ^Result calculator, applied to the leaves.
       -> (n -> m -> m) -- ^State updater, applied on non-leaves.
       -> m -- ^Initial state.
       -> T.Tree n
       -> [a]
leaves f g seed (Node n []) = [f n seed]
leaves f g seed (Node n xs) = concatMap (leaves f g (g n seed)) xs

-- |Collects just the leaves of a tree. Convenience function.
justLeaves :: (n -> a) -> T.Tree n -> [a]
justLeaves f = leaves (const . f) (const id) undefined
