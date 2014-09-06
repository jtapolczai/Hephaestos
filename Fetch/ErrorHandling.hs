{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Contains error-handling mechanisms built atop 'Control.Monad.Except'.
module Fetch.ErrorHandling where

import Control.Exception (catches, IOException, Handler(..))
import Control.Monad
import Control.Monad.Except
import Data.Either
import Data.Monoid

import Fetch.Types

-- |Converts an 'IOException' into a 'NetworkError' with the given
--  constructor. @fromIOException f = f . show@.
fromIOException :: (String -> NetworkError) -> IOException -> NetworkError
fromIOException f = f . show

-- |Lifts an IO action into the ExceptT IO transformer,
--  transforming all thrown IOExceptions into NetworkErrors.
catchIO :: (String -> NetworkError) -> IO a -> ErrorIO a
catchIO e m = liftIO m' >>= \x -> case x of (Right r) -> return r
                                            (Left l) -> throwError [l]
   where
      m' = liftM Right m `catches` [Handler (\(ex :: IOException) -> return $! Left $ e $ show ex)]

-- |Throws an error @e@, throws @[e]@.
addError :: MonadError [e] m => e -> m a
addError e = throwError [e]

-- |Prints an error to the stderr.
printError :: NetworkError -> ErrorIO ()
printError = liftIO . print

-- |Collects the errors from a list of results.
--  Defined as @return . mconcat . lefts@.
collectErrors :: (Monoid e, Monad m) => [Either e a] -> m e
collectErrors = return . mconcat . lefts

-- |Catches an error, performs an action, and re-throws it.
reportError :: MonadError e m => m a -> (e -> m b) -> m a
reportError m f = m `catchError` (\e -> f e >> throwError e)

-- |Analogous to 'guard'
guardErr :: MonadError e m => Bool -> e -> m ()
guardErr True = throwError
guardErr False = const $ return ()

-- |Version of @mapM_@ which collects all the errors which occur.
mapErr_ :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m e
mapErr_ f = mapErr f >=> collectErrors

-- |Version of @mapM@ which returns either the result of the function
--  or the error which occurred during its evaluation.
mapErr :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m [Either e b]
mapErr _ [] = return []
mapErr f (x:xs) = liftM2 (:) (liftM Right (f x) `catchError` (return . Left))
                             (mapErr f xs)
