-- Minimal Peano arithmetic module.
module Numeric.Peano where

-- |Lazy Peano numbers. Allow calculation with infinite values.
data Nat = Z | S Nat deriving (Show)

-- |Returns True iff a 'Nat' is Zero.
isZero :: Nat -> Bool
isZero Z = True
isZero _ = False

-- |Returns Truee iff a 'Nat' is non-Zero.
isSucc :: Nat -> Bool
isSucc = not . isZero

-- |Removes at most 'S' constructors from a Peano number.
--  Outputs the number of removed constructors and the remaining number.
takeNat :: (Num a, Enum a, Ord a) => a -> Nat -> (a, Nat)
takeNat = takeNat' 0
   where
      takeNat' c i n | i <= 0    = (c, n)
                     | isZero n  = (c, n)
                     | otherwise = takeNat' (succ c) (pred i) (pred n)

-- |The amount of natural numbers.
infinity :: Nat
infinity = S infinity

-- |Converts a number to Nat. Negative numbers are converted to Zero.
toNat :: (Num a, Ord a) => a -> Nat
toNat i | i <= 0    = Z
        | otherwise = S $ toNat $ i - 1

-- |Converts a finite 'Nat' to a number.
fromNat :: (Num a, Enum a) => Nat -> a
fromNat Z = 0
fromNat (S n) = succ $ fromNat n

-- |Enum-instance for 'Nat'. The 'pred' function is bounded at Zero.
instance Enum Nat where
   toEnum = toNat
   fromEnum = fromNat
   succ = S
   pred Z = Z
   pred (S n) = n

-- |Num-instance for 'Nat'. Addition, multiplication, and subtraction are
--  lazy in both arguments, meaning that, in the case of infinite values,
--  they can produce an infinite stream of S-constructors. As long as
--  the callers of these functions only consume a finite amount of these,
--  the program will not hang.
instance Num Nat where
   (+) Z n = n
   (+) n Z = n
   (+) (S n) (S m) = S $ S $ (+) n m

   (*) Z n = Z
   (*) n Z = Z
   (*) (S n) (S m) = S Z + n + m + (n * m)

   abs = id

   signum _ = S Z

   fromInteger = toNat

   (-) Z n = Z
   (-) n Z = n
   (-) (S n) (S m) = n - m

-- |Eq-instance for 'Nat'. '==' and '/=' work as long as at least one operand
--  is finite.
instance Eq Nat where
   (==) Z Z = True
   (==) Z (S _) = False
   (==) (S _) Z = False
   (==) (S n) (S m) = n == m

-- |Ord-instance for 'Nat'. All methods work as long as at least one operand
--  is finite.
instance Ord Nat where
   (<=) Z Z = True
   (<=) Z (S _) = True
   (<=) (S _) Z = False
   (<=) (S n) (S m) = n <= m

-- |Returns the length of a list as Nat. Can handle infinite lists.
natLength :: [a] -> Nat
natLength = natLength' Z
   where
      natLength' c [] = c
      natLength' c (_:xs) = natLength' (succ c) xs
