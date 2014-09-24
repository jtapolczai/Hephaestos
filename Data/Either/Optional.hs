{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |Generalization of 'Either' and 'Maybe' into the 'Optional' typeclass.
--  An instance of 'Optional' may contain 0 or 1 values.
module Data.Either.Optional (
   -- *Type class
   Optional(..),
   -- *Existential types
   --  |Given that it's often enough to know whether all elements in a
   --   heterogeneous collection have a value, two existentially quantified
   --   types are exported. 'Opt' hides the type variable of a @*->*@-kinded
   --   constructor and thus, the only thing one can do is call 'hasValue'
   --   on it. 'Opt'' only hides the first variable of a @*->*->*@-kinded
   --   constructor. This is useful when the error messages ere heterogeneous,
   --   but the actual values are not. Thus, both 'hasValue' and 'getValue'
   --   are available on it.
   Opt(..),
   Opt'(..),
   optHasValue,
   optHasValue',
   optValue,
   -- *Inverted Either
   -- |'Err' switches an 'Either's Left and Right. Combined with 'Opt'',
   --  this allows existential quantification over the value instead of the
   --  error message.
   Err(..),
   fromLeftErr,
   fromRightErr,
   -- *Turning 'Maybe' to 'Either'
   toEither,
   )where

import Control.Applicative (Applicative (..))
import Control.Arrow (left)
import Data.Either.Unwrap (fromLeft, fromRight)
import Data.Maybe
import Data.Typeable

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

-- |Turns a Maybe into an Either.
toEither :: a -> Maybe b -> Either a b
toEither x Nothing = Left x
toEither _ (Just x) = Right x

-- |Existentially quantified 'Optional' datatype.
data Opt = forall o v. (Optional o) => Opt{fromOpt::o v} -- ^Swallows an 'Optional''s value.

-- |Returns whether an 'Opt' has a value.
optHasValue :: Opt -> Bool
optHasValue (Opt x) = hasValue x

-- |Existentially quantified 'Either' which swallows the Left but keeps the Right
--  as an exposed type variable.
data Opt' w = forall o v. (Optional (o v)) => Opt' (o v w)

-- |Gets the value from an 'Opt'', if it exists. This function is partial.
optValue :: Opt' w -> w
optValue (Opt' x) = getValue x

-- |Returns whether an 'Opt'' has a value.
optHasValue' :: Opt' w -> Bool
optHasValue' (Opt' x) = hasValue x

-- |Inversion of 'Either' whose Left and Right are reversed. Putting such a value
--  into an 'Opt'' results in a type with an optional error.
data Err a b = Err (Either b a) deriving (Show, Eq, Read, Ord, Typeable)

-- |Gets the Right value from an Err.
fromLeftErr :: Err a b -> a
fromLeftErr (Err x) = fromRight x

-- |Gets the Left value from an Err.
fromRightErr :: Err a b -> b
fromRightErr (Err x) = fromLeft x

instance Functor (Err a) where
   fmap f (Err v) = Err $ left f v

instance Applicative (Err a) where
   pure a = Err $ Left a
   (Err (Left f)) <*> v = fmap f v
   (Err (Right r)) <*> _ = Err $ Right r

instance Monad (Err a) where
   return = pure
   (Err (Right r)) >>= _ = Err $ Right r
   (Err (Left l)) >>= f = f l

instance Optional (Err a) where
   hasValue (Err v) = not $ hasValue v
   getValue (Err (Left v)) = v

