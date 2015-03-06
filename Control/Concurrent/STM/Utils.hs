module Control.Concurrent.STM.Utils (
   -- * Task Limits (potentially infinite semaphores)
   TaskLimit,
   newTaskLimit,
   newTaskLimitIO,
   newMutex,
   addTask,
   removeTask,
   withTaskLimit,
   -- * Categories of tasks
   TaskCategories,
   makeCategories,
   getTasks,
   clearTasks,
   insertTask,
   updateTask,
   transferTask,
   -- *Other functions
   readWholeQueue,
   parMapSTM,
   atomically',
   fork,
   StartMarker,
   forkDelayed,
   ) where

import Prelude hiding (max)

import Control.Arrow (first)
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Exception (SomeException)
import Control.Monad.Catch (catch)
import Control.Monad.Loops (whileJust)
import Control.Monad.STM
import Control.Monad.Trans
import qualified Data.Collections as Co
import qualified Data.IntMap as IM
import Data.Functor.Monadic
import qualified Data.Map as M
import qualified System.Log.Logger as Log

debugM x = Log.debugM ("Hephaestos.STM.Utils." ++ x)


-- Task limits
-------------------------------------------------------------------------------

-- |Abstract type for a limit on the number of concurrent tasks.
--  This is a semaphore that might have an infinite value (no limit).
newtype TaskLimit = TaskLimit (TVar (Maybe Int))

-- |Creates a new 'TaskLimit'. 'Nothing' represents an infinite limit.
newTaskLimit :: Maybe Int -> STM TaskLimit
newTaskLimit = newTVar >=$> TaskLimit

-- |Creates a new 'TaskLimit' in the IO monad. Useful for creating top-level
--  TaskLimits.
newTaskLimitIO :: Maybe Int -> IO TaskLimit
newTaskLimitIO = newTVarIO >=$> TaskLimit

-- |Creates a task limit with an initial value of 1.
newMutex :: STM TaskLimit
newMutex = newTVar (Just 1) >$> TaskLimit

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
   where cats = M.fromList $ map (\k -> (k, IM.empty)) xs

-- |Gets all tasks.
getTasks :: TaskCategories cat v -> STM (M.Map cat (IM.IntMap v))
getTasks (TaskCategories t) = readTVar t >$> fst

-- |Removes all tasks of a given category.
clearTasks :: Ord cat => TaskCategories cat v -> cat -> STM ()
clearTasks (TaskCategories t) k = modifyTVar' t (first (M.insert k IM.empty))

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

-- |Lifted version of 'atomically'.
atomically' :: MonadIO m => STM a -> m a
atomically' = liftIO . atomically

-- |Represents the status of a task.
data TaskStatus = TaskBeginning | TaskRunning | TaskFinished
   deriving (Show, Eq, Ord, Enum)

-- |Forks a thread and puts its result into a TMVar when it finishes.
--  If the started thread throws an exception, it is caught
--  and put into the result.
--  In case the result is a 'Left', the thread should be considered dead and
--  the returned 'ThreadId' to be useless.
fork :: MonadIO m => IO a -> m (ThreadId, TMVar (Either SomeException a))
fork action = liftIO $ do
   box <- atomically $ newEmptyTMVar
   threadId <- forkIO $ catch (action >>= atomically . putTMVar box . Right)
                              (atomically . putTMVar box . Left)
   return (threadId, box)

-- |A function that is called to indicate that a task has started.
type StartMarker = STM ()

-- |Like 'fork', but only returns once an IO action explicitly signals that it
--  has begun. If an exception occurs before or after the intended return point,
--  it is put into the result.
--
--  For details, see 'fork'.
forkDelayed :: MonadIO m => (STM () -> IO a)
               -- ^The action that should be executed. The input of type
               --  @STM ()@ should be called to signal that that the task
               --  has started and that 'forkDelayed' may return. If it is
               --  never called, the thread will block indefinitely.
            -> m (ThreadId, TMVar (Either SomeException a))
               -- ^The ID of the started thread and the MVar into which the
               --  result will be put.
forkDelayed action = liftIO $ do
   status <- atomically $ newTVar TaskBeginning
   box <- atomically $ newEmptyTMVar
   threadId <- forkIO $ catch (do debugM "forkDelayed" "action starting."
                                  res <- action (writeTVar status TaskRunning)
                                  debugM "forkDelayed" "action finished."
                                  atomically $ do
                                     putTMVar box (Right res)
                                     writeTVar status TaskFinished
                                  debugM "forkDelayed" "STM executed.")
                              (\e -> atomically $ do putTMVar box (Left e)
                                                     writeTVar status TaskFinished)
   atomically $ readTVar status >>= check . (TaskBeginning <)
   return (threadId, box)
