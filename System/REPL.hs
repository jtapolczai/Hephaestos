{-# LANGUAGE OverloadedStrings #-}

-- |Functions to expedite the building of REPLs.
module System.REPL (
   -- *Text-versions of Prelude Functions
   putStrLnT,
   putStrT,
   putErrLnT,
   hPutStrLnT,
   hPutStrT,
   getLineT,
   -- **Lifted versions
   putStrLn,
   putStr,
   putErrLn,
   hPutStrLn,
   hPutStr,
   getLine,
   -- *Feture-rich reading of user-input
   prompt,
   prompt',
   askFor,
   askForWhen,
   -- *Command dispatch
   -- *Convenience functions for handling state
   -- These can be convenient when one wishes to 
   -- to extract a number of elements from the current state via pattern
   -- -matching, e.g.
   --
   -- @
   -- data State = State{f::a,g::b,h::c}
   --
   -- do (x,z) <- get2 f h
   --    ...do something with x and z...
   -- @
   get1,
   get2,
   get3,
   get4,
   get5,
   get6,
   get7,
   get8,
   ) where

import Prelude hiding (putStrLn, putStr, getLine)
import qualified Prelude as P

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Text
import System.IO hiding (putStrLn, putStr, getLine)
import Text.Read (readMaybe)

-- |'Text'-analogue of 'putStrLn'.
putStrLnT :: Text -> IO ()
putStrLnT = P.putStrLn . unpack

-- |'Text'-analogue of 'putStr'.
putStrT :: Text -> IO ()
putStrT = P.putStr . unpack

-- |'Text'-analogue of 'hPutStrLn stderr'
putErrLnT :: Text -> IO ()
putErrLnT = System.IO.hPutStr stderr . unpack

-- |'Text'-analogue of 'getLine'.
getLineT :: IO Text
getLineT = liftM pack P.getLine

-- |'Text'-analogue of 'hPutStrLn'.
hPutStrLnT :: Handle -> Text -> IO ()
hPutStrLnT h = System.IO.hPutStrLn h . unpack

-- |'Text'-analogue of 'hPutStrLn'.
hPutStrT :: Handle -> Text -> IO ()
hPutStrT h = System.IO.hPutStr h . unpack

-- |Prints @> @ and asks the user to input a line.
prompt :: IO Text
prompt = prompt' "> "

-- |Prints its first argument and, in the same line, asks the user
--  to input a line.
prompt' :: Text -> IO Text
prompt' s = putStrT s >> hFlush stdout >> getLineT

-- |Lifted version of 'putStrLnT'.
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . putStrLnT

-- |Lifted version of 'putStrT'.
putStr :: MonadIO m => Text -> m ()
putStr = liftIO . putStrT

-- |Lifted version of 'getLineT'.
getLine :: MonadIO m => m Text
getLine = liftIO getLineT

-- |Lifted version of 'putErrLnT'
putErrLn :: MonadIO m => Text -> m ()
putErrLn = liftIO . putErrLnT

-- |Asks the user for an input of a given type.
--  If the input is of the wrong type, an error-message is printed
--  and the user is asked again.
--  This function is intended to be used like this:
--
--  @
--  (var::Int) <- askFor prompt errorMessage
--  @
askFor :: (MonadIO m, Read a)
       => Text -- ^The prompt.
       -> Text -- ^The error message in case the read of the input fails.
       -> m a
askFor pr err = askForWhen (const $ return True) pr err undefined

-- |Asks the user for an input of a given type.
--  If the input is of the wrong type, an error-message is printed
--  and the user is asked again.
--  In addition to the condition that the input must be of the correct
--  type, it must also fulfill a predicate.
--
--  Since the predicate is of type @a -> m Bool@, arbitrarily complex
--  tests can be performed: checking whether an item is in a database,
--  whether a date was less than x years ago, etc.
askForWhen :: (MonadIO m, Read a)
           => (a -> m Bool) -- ^The predicate which the input must fulfill.
           -> Text -- ^The prompt.
           -> Text -- ^The error message in case the read of the input fails.
           -> Text -- ^The error message in case the read succeeds but the
                   -- ^predicate returns false.
           -> m a
askForWhen check pr errType errCheck =
   let
      rec = askForWhen check pr errType errCheck
   in
   do y <- liftIO $ prompt' pr
      let x = readMaybe $ unpack y
      case x of Nothing -> putErrLn errType >> rec
                (Just x') -> do res <- check x'
                                if res then return x'
                                else putErrLn errCheck >> rec



-- |Extracts a result from the current state.
--  Defined as @get1 f = liftM f get@.
get1 :: Monad m
     => (s -> a)
     -> StateT s m a
get1 f1 = liftM f1 get

-- |Extracts two results from the current state.
get2 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> StateT s m (a,b)
get2 f1 f2 = liftM (f1 &&& f2) get

-- |Extracts three results from the current state.
get3 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> StateT s m (a,b,c)
get3 f1 f2 f3 = liftM (\x -> (f1 x,f2 x, f3 x)) get

-- |Extracts four results from the current state.
get4 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> StateT s m (a,b,c,d)
get4 f1 f2 f3 f4 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x)) get

-- |Extracts five results from the current state.
get5 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> StateT s m (a,b,c,d,e)
get5 f1 f2 f3 f4 f5 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x)) get

-- |Extracts six results from the current state.
get6 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> StateT s m (a,b,c,d,e,f)
get6 f1 f2 f3 f4 f5 f6 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x)) get

-- |Extracts seven results from the current state.
get7 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> (s -> g)
     -> StateT s m (a,b,c,d,e,f,g)
get7 f1 f2 f3 f4 f5 f6 f7 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x, f7 x)) get

-- |Extracts eight results from the current state.
get8 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> (s -> g)
     -> (s -> h)
     -> StateT s m (a,b,c,d,e,f,g,h)
get8 f1 f2 f3 f4 f5 f6 f7 f8 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x, f7 x, f8 x)) get

