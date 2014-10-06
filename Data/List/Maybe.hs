-- |Operations on list. This module re-exports all safe functions of
--  'Data.List', but wraps all functions which require non-empty lists
--  into 'Maybe's.
module Data.List.Maybe (
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
   -- * Generic wrapper function.
   wrap,
   )where

import Prelude hiding (head, tail, init, last, foldl1, foldr1, maximum, minimum)

import qualified Data.List as L
import qualified Data.List as LSafe hiding (head, last, tail, init, foldl1, foldl1', foldr1, maximum, minimum, maximumBy, minimumBy)

-- |Takes a function that requires a non-empty list and wraps it in a 'Maybe'.
wrap :: ([a] -> b) -> [a] -> Maybe b
wrap _ [] = Nothing
wrap f xs = Just $ f xs

-- |Extract the first element of a list. Empty lists return 'Nothing'.
head :: [a] -> Maybe a
head = wrap L.head

-- |Extract the last element of a list. Empty lists return 'Nothing'.
last :: [a] -> Maybe a
last = wrap L.last

-- |Extract the elements after the head of a list. Empty lists return 'Nothing'.
tail :: [a] -> Maybe [a]
tail = wrap L.tail

-- |Return all the elements of a list except the last one.
--  Empty lists return 'Nothing'.
init :: [a] -> Maybe [a]
init = wrap L.init

-- |'foldl1' is a variant of 'foldl' that has no starting value, and thus must
--  be applied to non-empty lists. Empty lists return 'Nothing'.
foldl1 :: (a -> a -> a) -> [a] -> Maybe a
foldl1 = wrap . L.foldl1

-- |A strict version of 'foldl1'.
foldl1' :: (a -> a -> a) -> [a] -> Maybe a
foldl1' = wrap . L.foldl1'

-- |'foldr1' is a variant of 'foldr' that has no starting value, and thus must
--  be applied to non-empty lists. Empty lists return 'Nothing'.
foldr1 :: (a -> a -> a) -> [a] -> Maybe a
foldr1 = wrap . L.foldr1

-- |'maximum' returns the maximum value from a list, which must be non-empty,
--  finite, and of an ordered type. It is a special case of 'maximumBy', which
--  allows the programmer to supply their own comparison function.
--  Empty lists return 'Nothing'.
maximum :: Ord a => [a] -> Maybe a
maximum = wrap L.maximum

-- |'minimum' returns the maximum value from a list, which must be non-empty,
--  finite, and of an ordered type. It is a special case of 'minimumBy', which
--  allows the programmer to supply their own comparison function.
--  Empty lists return 'Nothing'.
minimum :: Ord a => [a] -> Maybe a
minimum = wrap L.minimum

-- |The 'maximumBy' function takes a comparison function and a list and returns
--  the greatest element of the list by the comparison function. The list must
--  be finite and non-empty. Empty lists return 'Nothing'.
maximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy = wrap . L.maximumBy

-- |The 'minimumBy' function takes a comparison function and a list and returns
--  the least element of the list by the comparison function. The list must
--  be finite and non-empty. Empty lists return 'Nothing'.
minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy = wrap . L.minimumBy
