{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Contains error-handling mechanisms built atop 'Control.Monad.Except'.
module Crawling.Hephaestos.Fetch.ErrorHandling where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Either (lefts)
import Data.Either.Unwrap
import Data.Monoid
import qualified Data.Foldable as Fd (Foldable, mapM_)
import Data.Text (Text, pack)
import System.REPL (putErrLn)

import Crawling.Hephaestos.Fetch.Types

-- |Converts an 'IOException' into a 'NetworkError' with the given
--  constructor. @fromIOException f = f . show@.
fromIOException :: (Text -> NetworkError) -> IOException -> NetworkError
fromIOException f = f . pack . show

-- |Lifts an IO action into the ExceptT IO transformer,
--  transforming all thrown IOExceptions into NetworkErrors.
catchIO :: (MonadError SomeException m, MonadIO m)
        => Text -> (Text -> NetworkErrorKind) -> IO a -> m a
catchIO u e m = liftIO m' >>= \x -> case x of (Right r) -> return r
                                              (Left l) -> throwError l
   where
      m' = liftM Right m `catch`
           (\(ex :: IOException) -> return $! Left
                                           $ SomeException
                                           $ NetworkError u
                                           $ e
                                           $ pack
                                           $ show ex)


-- |Lifts an IO action into the ExceptT IO transformer,
--  transforming all thrown HttpExceptions into NetworkErrors.
catchHttp :: (MonadIO m, MonadError SomeException m)
          => Text -> IO a -> m a
catchHttp u m = liftIO m' >>= \x -> case x of (Right r) -> return r
                                              (Left l) -> addError l
   where
      m' = liftM Right m `catches`
           [Handler (\(ex :: HttpException) ->
                      return $! Left $ SomeException $ NetworkError u $ HttpError ex)]

-- |Given an error @e@, throws @[e]@.
addError :: (Exception e, MonadError SomeException m) => e -> m a
addError e = throwError $ SomeException [e]

-- |Convenience function for throwing 'NetworkError's.
--  @addNetworkError s k = throwError (NetworkError s k)@.
addNetworkError :: MonadError SomeException m => URL -> NetworkErrorKind -> m a
addNetworkError s k = throwError $ SomeException $ NetworkError s k

-- |Prints an error with 'System.REPL.putErrLn'.
printError :: (MonadIO m, Show a) => a -> m ()
printError = putErrLn . show

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
                        let ys' = if isRight pr && fromRight pr then x : ys
                                                                else ys
                            es' = if isLeft pr then fromLeft pr : es
                                               else es
                        return (ys',es')
