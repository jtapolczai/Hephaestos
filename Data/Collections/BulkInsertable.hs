{-# LANGUAGE MultiParamTypeClasses #-}

-- |Collections which support efficient bulk insertion.
module Data.Collections.BulkInsertable where

import qualified Data.Set as S

-- |The class of data structures to which can be appended.
class BulkInsertable a b where
   bulkInsert :: a -> b -> b

instance Ord a => BulkInsertable [a] (S.Set a) where
   bulkInsert xs ys = S.fromList xs `S.union` ys

instance Ord a => BulkInsertable (S.Set a) (S.Set a) where
   bulkInsert = S.union

instance BulkInsertable [a] [a] where
   bulkInsert = (++)

