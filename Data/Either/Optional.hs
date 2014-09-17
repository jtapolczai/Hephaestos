{-# LANGUAGE ExistentialQuantification #-}

-- |Generalization of 'Either' and 'Maybe' into the 'Optional' typeclass.
module Data.Either.Optional where

import Data.Maybe

-- |The class of types with optional content.
class Optional o where
   hasValue :: o v -> Bool
   getValue :: o v -> v

instance Optional Maybe where
   hasValue = isJust
   getValue = fromJust

instance Optional (Either a) where
   hasValue (Left _) = False
   hasValue (Right _) = True
   getValue (Right r) = r

data Opt = forall o v. (Optional o) => Opt (o v)

allHasValue :: [Opt] -> Bool
allHasValue = all (\(Opt x) -> hasValue x)