{-# LANGUAGE ExistentialQuantification #-}

-- |Generalization of 'Either' and 'Maybe' into the 'Optional' typeclass. An instance of 'Optional' may
--  contain 0 or 1 values.
module Data.Either.Optional where

import Data.Maybe

-- |The class of types with optional content.
class Optional o where
   -- |Indicates whether the container has a value it in.
   hasValue :: o v -> Bool
   -- |Get the container's value, if present. The result is undefined if there is no value.
   getValue :: o v -> v

-- |Instance for 'Maybe'.
instance Optional Maybe where
   hasValue = isJust
   getValue = fromJust

-- |Right-instance for 'Either'.
instance Optional (Either a) where
   hasValue (Left _) = False
   hasValue (Right _) = True
   getValue (Right r) = r

-- |Existentially quantified 'Optional' datatype.
data Opt = forall o v. (Optional o) => Opt (o v) -- ^Swallows an 'Optional''s value.

-- |Returns True iff all elements have a value.
allHaveValue :: [Opt] -> Bool
allHaveValue = all (\(Opt x) -> hasValue x)

-- |Returns True iff at least one element has a value.
anyHaveValue :: [Opt] -> Bool
anyHaveValue = any  (\(Opt x) -> hasValue x)

-- |Maps 'hasValue' onto all elements.
mapHasValue :: Functor f => f Opt -> f Bool
mapHasValue = fmap (\(Opt x) -> hasValue x)