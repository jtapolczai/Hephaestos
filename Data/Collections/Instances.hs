{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |Additional collection instances for common data structures.
module Data.Collections.Instances where

import qualified Data.Collections as Co
import qualified Data.Collections.Foldable as Co
import qualified Data.Map as M
import qualified Data.Set as S

instance Co.Unfoldable [a] a where
   insert = (:)
   empty = []
   singleton = (:[])

instance Co.Collection [a] a where
   filter = filter

instance Ord a => Co.Unfoldable (S.Set a) a where
   insert = S.insert
   empty = S.empty
   singleton = S.singleton

instance Ord a => Co.Map (S.Set a) a () where
   delete = S.delete
   member = S.member
   union = S.union
   intersection = S.intersection
   difference = S.difference
   isSubset = S.isSubsetOf
   isProperSubset = S.isProperSubsetOf
   lookup a s = if S.member a s then Just () else Nothing
   alter _ _ s = s
   mapWithKey _ s = s
   unionWith _ = S.union
   intersectionWith _ = S.intersection
   differenceWith _ = error "differenceWith undefined for sets."
   isSubmapBy = error "isSubmapBy undefined for sets."
   isProperSubmapBy = error "isProperSubmapBy undefined for sets."

instance Ord k => Co.Unfoldable (M.Map k v) (k,v) where
   insert = uncurry M.insert
   empty = M.empty
   singleton = uncurry M.singleton

instance (Ord k) => Co.Map (M.Map k v) k v where
   delete = M.delete
   member = M.member
   union = M.union
   intersection = M.intersection
   difference = M.difference
   isSubset  = M.isSubmapOfBy (const $ const True)
   isProperSubset = M.isProperSubmapOfBy (const $ const True)
   lookup = M.lookup
   alter = M.alter
   mapWithKey = M.mapWithKey
   unionWith = M.unionWith
   intersectionWith = M.intersectionWith
   differenceWith = M.differenceWith
   isSubmapBy = M.isSubmapOfBy
   isProperSubmapBy = M.isProperSubmapOfBy
