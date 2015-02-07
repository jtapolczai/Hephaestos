{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tree.Monadic where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Monad as C
import Control.Monad.STM
import Data.Functor.FunctorM
import Data.Functor.Monadic
import Data.Tree as T

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
materializePar :: Int
                  -- ^The upper limit on simultaneous tasks.
                  --  A value of 1 results in an ordering of operations
                  --  identical to that of 'materialize'; a value of below
                  --  0 will cause non-termination.
               -> MTree IO n
               -> IO (Tree n)
materializePar n tree = do numTasks <- atomically $ newTVar n
                           materializePar' numTasks tree
   where
      materializePar' numTasks (MTree m) = do
         (MNode v children) <- m
         -- create a queue to store the children's results
         resQ <- atomically $ newTQueue
         -- create a countdown for the number of processed children
         childRes <- atomically $ newTVar $ length children
         -- spark threads for the children
         mapM (f resQ numTasks) children
         -- block until all children are finished
         atomically (readTVar childRes >>= check . (<=0))
         -- collect the results and return
         results <- atomically $ readWholeQueue resQ
         return $ Node v results


      -- |Starts a new instance of materializePar'.
      --  Blocks as long as q's value is <= 0.
      f res q node = do atomically $ addTask q
                        forkIO (do nodeRes <- materializePar' q node
                                   atomically (do writeTQueue res nodeRes
                                                  removeTask q))

      -- |Reads all elements of a queue.
      readWholeQueue :: TQueue a -> STM [a]
      readWholeQueue q = do c <- isEmptyTQueue q
                            if c then return []
                                 else do el <- readTQueue q
                                         rest <- readWholeQueue q
                                         return $ el:rest

      -- |Decreases a value by 1 and blocks if the result would be below 0.
      addTask :: TVar Int -> STM ()
      addTask v = do v' <- readTVar v
                     if v' <= 0 then retry else writeTVar v (v'-1)

      -- |Increases a value by one.
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
