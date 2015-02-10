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

-- |Collects the errors from a list of results.
--  Defined as @return . mconcat . lefts@.
collectErrors :: (Monad m) => [Either e a] -> m [e]
collectErrors = return . lefts

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
