module Control.Concurrent.STM.Utils where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Monad.Loops (whileJust)
import Control.Monad.STM
import qualified Data.Collections as Co
import qualified Data.Collections.BulkInsertable as Co
import qualified Data.Collections.Instances as Co
import Data.Functor.Monadic

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

-- |Performs an action if a semaphore is free (>0).
--  The TVar is decremented for the duration of the action.
--
--  Note: this does not spawn a new thread.
withSemaphore :: TVar Int -> IO a -> IO a
withSemaphore lock m = do atomically $ addTask lock
                          res <- m
                          atomically $ removeTask lock
                          return res

-- |Executes a collection of actions with a thread pool,
--  gathering the results in a qeue.
--
--  The function blocks until all actions have finished.
--  Ordering of the results is not guaranteed;
--  they are inserted as each task finishes.
withThreadPool :: (Co.Foldable (f a) a, Co.Unfoldable (g b) b,
                   Co.Unfoldable (f a) a)
               => (a -> IO b)
                  -- ^The function which is to be applied to each element
                  --  of the input.
               -> f a
                  -- ^Input data.
               -> IO (g b)
withThreadPool f inp = do
   remaining <- atomically $ newTVar $ Co.size inp
   results <- atomically newTQueue
   Co.traverse_ (f' remaining results) inp
   atomically (readTVar remaining >>= check . (<=0))
   atomically (readWholeQueue results >$> flip Co.insertMany Co.empty)

   where
      f' remaining results n =
         forkIO (do result <- f n
                    atomically (writeTQueue results result
                                >> modifyTVar remaining (subtract 1)))
