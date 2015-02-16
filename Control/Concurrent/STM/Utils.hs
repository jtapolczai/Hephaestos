module Control.Concurrent.STM.Utils (
   -- * Task Limits (potentially infinite semaphores)
   TaskLimit,
   newTaskLimit,
   addTask,
   removeTask,
   withTaskLimit,
   -- * Categories of tasks
   TaskCategories,
   makeCategories,
   insertTask,
   updateTask,
   transferTask,
   -- *Other functions
   readWholeQueue,
   parMapSTM,
   ) where

import Prelude hiding (max)

import Control.Arrow (first)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Monad ((>=>))
import Control.Monad.Loops (whileJust)
import Control.Monad.STM
import Control.Monad.Trans
import qualified Data.Collections as Co
import qualified Data.IntMap as IM
import Data.Functor.Monadic
import qualified Data.Map as M

-- Task limits
-------------------------------------------------------------------------------

-- |Abstract type for a limit on the number of concurrent tasks.
--  This is a semaphore that might have an infinite value (no limit).
newtype TaskLimit = TaskLimit (TVar (Maybe Int))

-- |Creates a new 'TaskLimit'. 'Nothing' represents an infinite limit.
newTaskLimit :: Maybe Int -> STM TaskLimit
newTaskLimit = newTVar >=> return . TaskLimit

-- |Descreases a task limit by 1, if it exists. If the value is already at
--  0, the function blocks.
addTask :: TaskLimit -> STM ()
addTask (TaskLimit v) = do
   v' <- readTVar v
   case v' of Nothing  -> return ()
              Just v'' -> if v'' <= 0 then retry
                                      else writeTVar v (fmap (subtract 1) v')

-- |Increases a value by one. A semaphore's "signal"-
removeTask :: TaskLimit -> STM ()
removeTask (TaskLimit l) = modifyTVar' l (fmap (+1))

-- |Performs an IO action, taking into account a TaskLimit, i.e.
--  blocking while it is @<=0@ and then temporarily decrementing it.
--
--  Note: this does not spawn a new thread.
withTaskLimit :: MonadIO m => TaskLimit -> m a -> m a
withTaskLimit lock m = do liftIO $ atomically $ addTask lock
                          res <- m
                          liftIO $ atomically $ removeTask lock
                          return res

-- Download slots
-------------------------------------------------------------------------------

-- |A collection of categories, each of which contains a collection of
--  @Int@-indexed items.
newtype TaskCategories k v = TaskCategories (TVar (M.Map k (IM.IntMap v), Int))

-- |Create a new set of empty task categories.
makeCategories :: Ord cat => [cat] -> STM (TaskCategories cat v)
makeCategories xs = newTVar (cats,0) >$> TaskCategories
   where cats = M.fromList $ map (\k -> (k,IM.empty)) xs

-- |Insert a new task into the given category. The new task is given a fresh
--  Int as a key (fresh w.r.t. to the given 'TaskCategories' argument).
--  If the category with the given name does not exist, it is created.
insertTask :: Ord cat => TaskCategories cat v -> cat -> v -> STM Int
insertTask (TaskCategories t) cat v = do
   (t',max) <- readTVar t
   let newCat = IM.fromList [(max, v)]
       f _ = IM.insert max v
       max' = max+1
   writeTVar t $ (M.insertWith f cat newCat t', seq max' max')
   return max

-- |Updates a task in the given category and with the given key, if it exists.
--  If the task does not exist, nothing is done.
updateTask :: Ord cat => TaskCategories cat v -> cat -> Int -> (v -> v) -> STM ()
updateTask (TaskCategories t) cat k f =
   modifyTVar' t (first $ M.adjust (IM.adjust f k) cat)

-- |Moves a task from category to another, if it exists. If the task does not
--  exist, nothing is done.
transferTask :: Ord cat => TaskCategories cat v
             -> cat
             -- ^The old category
             -> Int
             -> cat
             -- ^The new category
             -> STM ()
transferTask (TaskCategories t) oldCat key newCat = do
   (t',max) <- readTVar t
   case M.lookup oldCat t' >$> IM.updateLookupWithKey (\_ _ -> Nothing) key of
      Just (Just v, lessMap) ->  let t'' = M.adjust (IM.insert key v) newCat
                                           $ M.insert oldCat lessMap t'
                                 in writeTVar t (t'', max)
      _ -> return ()

-- |Reads all elements of a queue.
readWholeQueue :: TQueue a -> STM [a]
readWholeQueue q = whileJust (tryReadTQueue q) return

-- |Executes a collection of actions with a thread pool,
--  gathering the results in a qeue.
--
--  The function blocks until all actions have finished.
--  Ordering of the results is not guaranteed;
--  they are inserted as each task finishes.
--
--  This function is morally identical to 'Control.Parallel.Strategies.parMap'
--  in Control.Parallel.Strategies, with the difference that it takes
--  an @a -> IO b@ instead of @a -> b@.
parMapSTM :: (Co.Foldable (f a) a, Co.Unfoldable (g b) b,
              Co.Unfoldable (f a) a)
          => (a -> IO b)
             -- ^The function which is to be applied to each element
             --  of the input.
          -> f a
             -- ^Input data.
          -> IO (g b)
parMapSTM f inp = do
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
