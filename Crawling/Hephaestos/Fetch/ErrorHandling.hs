{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Contains error-handling mechanisms built atop 'Control.Monad.Except'.
module Crawling.Hephaestos.Fetch.ErrorHandling where

import Control.Exception (Exception, SomeException(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Either (lefts)
import Data.Either.Combinators
import qualified Data.Foldable as Fd (Foldable, mapM_)
import Data.Typeable (cast)
import System.REPL (putErrLn)

import Crawling.Hephaestos.Fetch.Types

-- |Prints an error with 'System.REPL.putErrLn'.
printError :: (MonadIO m, Show a) => a -> m ()
printError = liftIO . putErrLn . show

-- |Prints a collection of errors with 'System.REPL.putErrLn'
printErrors :: (MonadIO m, Fd.Foldable f, Show a) => f a -> m ()
printErrors = Fd.mapM_ printError

-- |Collects the errors from a list of results.
--  Defined as @return . mconcat . lefts@.
collectErrors :: (Monad m) => [Either e a] -> m [e]
collectErrors = return . lefts

-- |Catches an error, performs an action, and re-throws it.
reportError :: (MonadCatch m, Exception e) => m a -> (e -> m b) -> m a
reportError m f = m `catch` (\e -> f e >> throwM e)

-- |Analogous to 'guard'
guardErr :: (MonadCatch m, Exception e) => Bool -> e -> m ()
guardErr True = throwM
guardErr False = const $ return ()

-- |Version of 'mapM_' which collects all the errors which occur.
mapErr_ :: (MonadCatch m, Exception e) => (a -> m b) -> [a] -> m [e]
mapErr_ f = mapErr f >=> collectErrors

-- |Version of 'mapM' which returns either the result of the function
--  or the error which occurred during its evaluation.
mapErr :: (MonadCatch m, Exception e) => (a -> m b) -> [a] -> m [Either e b]
mapErr _ [] = return []
mapErr f (x:xs) = liftM2 (:) (liftM Right (f x) `catch` (return . Left))
                             (mapErr f xs)

-- |Version of 'filterM' which collects all the errors encountered.
--  If @f x = False@ or @f x@ throws an error (for any x in the list),
--  it is filtered out.
filterErr :: (MonadCatch m, Exception e) => (a -> m Bool) -> [a] -> m ([a],[e])
filterErr _ [] = return ([],[])
filterErr f (x:xs) = do pr <- liftM Right (f x) `catch` (return . Left)
                        (ys,es) <- filterErr f xs
                        let ys' = if fromRight False pr then x : ys
                                                        else ys
                            es' = if isLeft pr then fromLeft' pr : es
                                               else es
                        return (ys',es')
