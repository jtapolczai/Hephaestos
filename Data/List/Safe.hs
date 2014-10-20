{-# LANGUAGE DeriveDataTypeable #-}

-- |Operations on list. This module re-exports all safe functions of
--  'Data.List', but wraps all functions which require non-empty lists
--  into an instance of 'MonadThrow'.
--  Among others, this includes @[]@, @Maybe@, @Either@ and @IO@. See
--  'Control.Monad.Catch'.
module Data.List.Safe (
   module LSafe,
   -- *Safe versions of standard functions.
   head,
   last,
   tail,
   init,
   foldl1,
   foldl1',
   foldr1,
   maximum,
   minimum,
   maximumBy,
   minimumBy,
   (!!),
   -- * Generic wrapper function.
   wrap,
   -- * Exceptions for empty lists and negative indices.
   EmptyListException,
   NegativeIndexException,
   )where

import Prelude hiding (head, tail, init, last, foldl1, foldr1, maximum, minimum, (!!))

import Control.Monad.Catch
import qualified Data.List as L
import Data.List as LSafe hiding (head, last, tail, init, foldl1, foldl1', foldr1, maximum, minimum, maximumBy, minimumBy, (!!))
import Data.Typeable

data EmptyListException = EmptyListException deriving (Show, Read, Eq, Ord, Typeable)
data NegativeIndexException = NegativeIndexException deriving (Show, Read, Eq, Ord, Typeable)

instance Exception EmptyListException
instance Exception NegativeIndexException


-- |Takes a function that requires a non-empty list and wraps it in an instance
--  of 'MonadThrow'.
wrap :: MonadThrow m => ([a] -> b) -> [a] -> m b
wrap _ [] = throwM EmptyListException
wrap f xs = return $ f xs

-- |Extract the first element of a list. Empty lists throw an 'EmptyListException'.
head :: MonadThrow m => [a] -> m a
head = wrap L.head

-- |Extract the last element of a list. Empty lists throw an 'EmptyListException'.
last :: MonadThrow m => [a] -> m a
last = wrap L.last

-- |Extract the elements after the head of a list.
--  Empty lists throw an 'EmptyListException'.
tail :: MonadThrow m => [a] -> m [a]
tail = wrap L.tail

-- |Return all the elements of a list except the last one.
--  Empty lists throw an 'EmptyListException'.
init :: MonadThrow m => [a] -> m [a]
init = wrap L.init

-- |'foldl1' is a variant of 'foldl' that has no starting value, and thus must
--  be applied to non-empty lists. Empty lists throw an 'EmptyListException'.
foldl1 :: MonadThrow m => (a -> a -> a) -> [a] -> m a
foldl1 = wrap . L.foldl1

-- |A strict version of 'foldl1'.
foldl1' :: MonadThrow m => (a -> a -> a) -> [a] -> m a
foldl1' = wrap . L.foldl1'

-- |'foldr1' is a variant of 'foldr' that has no starting value, and thus must
--  be applied to non-empty lists. Empty lists throw an 'EmptyListException'.
foldr1 :: MonadThrow m => (a -> a -> a) -> [a] -> m a
foldr1 = wrap . L.foldr1

-- |'maximum' returns the maximum value from a list, which must be non-empty,
--  finite, and of an ordered type. It is a special case of 'maximumBy', which
--  allows the programmer to supply their own comparison function.
--  Empty lists throw an 'EmptyListException'.
maximum :: (MonadThrow m, Ord a) => [a] -> m a
maximum = wrap L.maximum

-- |'minimum' returns the maximum value from a list, which must be non-empty,
--  finite, and of an ordered type. It is a special case of 'minimumBy', which
--  allows the programmer to supply their own comparison function.
--  Empty lists throw an 'EmptyListException'.
minimum :: (MonadThrow m, Ord a) => [a] -> m a
minimum = wrap L.minimum

-- |The 'maximumBy' function takes a comparison function and a list and returns
--  the greatest element of the list by the comparison function. The list must
--  be finite and non-empty. Empty lists throw an 'EmptyListException'.
maximumBy :: MonadThrow m => (a -> a -> Ordering) -> [a] -> m a
maximumBy = wrap . L.maximumBy

-- |The 'minimumBy' function takes a comparison function and a list and returns
--  the least element of the list by the comparison function. The list must
--  be finite and non-empty. Empty lists throw an 'EmptyListException'.
minimumBy :: MonadThrow m => (a -> a -> Ordering) -> [a] -> m a
minimumBy = wrap . L.minimumBy

-- |List index (subscript) operator, starting from 0. Indices larger than
--  @length xs - 1@ throw an 'EmptyListException', negative indices throw
--  an 'NegativeIndexException'.
(!!) :: (MonadThrow m, Integral n, Ord n) => [a] -> n -> m a
(!!) [] _ = throwM EmptyListException
(!!) (x:xs) n | n == 0 = return x
              | n < 0 = throwM NegativeIndexException
              | otherwise = (!!) xs (n-1)
