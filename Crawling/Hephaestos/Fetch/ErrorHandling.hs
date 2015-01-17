{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Contains error-handling mechanisms built atop 'Control.Monad.Except'.
module Crawling.Hephaestos.Fetch.ErrorHandling where

import Control.Exception (Exception, SomeException(..))
import Control.Monad
import Control.Monad.Except
import Data.Either (lefts)
import Data.Either.Combinators
import qualified Data.Foldable as Fd (Foldable, mapM_)
import Data.Typeable (cast)
import System.REPL (putErrLn)

import Crawling.Hephaestos.Fetch.Types

-- |Lifts an IO action into the ExceptT IO transformer,
--  catching any thrown exceptions and re-throwing them as SomeException.
--  Use this function instead of 'liftIO' when working with IO-type actions
--  in the ExceptT-monad.
--
--  ExceptT ordinarily can't catch IOExceptions and the like, because
--  its MonadCatch-instance is @MonadCatch SomeException (ExceptT m)@.
catchIO :: (MonadError SomeException m, MonadIO m) => IO a -> m a
catchIO m = liftIO m' >>= either throwError return
   where
      m' = liftM Right m `catch`
           (\(ex :: SomeException) -> return $! Left (SomeException ex))

-- |Variant of 'Control.Exdeption.catches' that's polymorphic over the used monad.
catches :: (Exception e, MonadError e m) => m a -> [Handler m a] -> m a
catches m handlers = m `catchError` firstJust handlers
   where
      firstJust [] e = throwError e
      firstJust ((Handler x):xs) e = maybe (firstJust xs e) x (cast e)

-- |Variant of 'Control.Exdeption.catch' that's polymorphic over the used monad.
catch :: (Exception e, Exception f, MonadError e m) => m a -> (f -> m a) -> m a
catch m handler = catches m [Handler handler]

-- |Exception-handler for a monad.
data Handler m a = forall e. Exception e => Handler (e -> m a)

-- |Prints an error with 'System.REPL.putErrLn'.
printError :: (MonadIO m, Show a) => a -> m ()
printError = liftIO . putErrLn . show

-- |Prints a collection of errors with 'System.REPL.putErrLn'
printErrors :: (MonadIO m, Fd.Foldable f, Show a) => f a -> m ()
printErrors = Fd.mapM_ printError

-- |Runs an Exception monad and prints out the errors with 'printErrors'.
--  Also conveniently lifts the result.
runExceptT' :: (MonadIO m, MonadIO (t m), MonadTrans t, Exception e)
             => ExceptT e m a -> t m (Either e a)
runExceptT' m = lift $ do res <- runExceptT m
                          case res of
                               Right r -> return (Right r)
                               Left l -> printError l >> return (Left l)

-- |Collects the errors from a list of results.
--  Defined as @return . mconcat . lefts@.
collectErrors :: (Monad m) => [Either e a] -> m [e]
collectErrors = return . lefts

-- |Catches an error, performs an action, and re-throws it.
reportError :: MonadError e m => m a -> (e -> m b) -> m a
reportError m f = m `catchError` (\e -> f e >> throwError e)

-- |Analogous to 'guard'
guardErr :: MonadError e m => Bool -> e -> m ()
guardErr True = throwError
guardErr False = const $ return ()

-- |Version of 'mapM_' which collects all the errors which occur.
mapErr_ :: (MonadError e m) => (a -> m b) -> [a] -> m [e]
mapErr_ f = mapErr f >=> collectErrors

-- |Version of 'mapM' which returns either the result of the function
--  or the error which occurred during its evaluation.
mapErr :: MonadError e m => (a -> m b) -> [a] -> m [Either e b]
mapErr _ [] = return []
mapErr f (x:xs) = liftM2 (:) (liftM Right (f x) `catchError` (return . Left))
                             (mapErr f xs)

-- |Version of 'filterM' which collects all the errors encountered.
--  If @f x = False@ or @f x@ throws an error (for any x in the list),
--  it is filtered out.
filterErr :: (MonadError e m) => (a -> m Bool) -> [a] -> m ([a],[e])
filterErr _ [] = return ([],[])
filterErr f (x:xs) = do pr <- liftM Right (f x) `catchError` (return . Left)
                        (ys,es) <- filterErr f xs
                        let ys' = if fromRight False pr then x : ys
                                                        else ys
                            es' = if isLeft pr then fromLeft' pr : es
                                               else es
                        return (ys',es')
