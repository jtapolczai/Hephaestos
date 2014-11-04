-- Value-level Peano arithmetic.
module Numeric.Peano where

import Data.Ratio ((%))

-- |Lazy Peano numbers. Allow calculation with infinite values.
data Nat = Z | S Nat deriving (Show)

-- |Sign for whole numbers.
data Sign = Pos | Neg deriving (Show, Eq, Ord, Enum, Read, Bounded)

-- |Whole numbers (Z).
data Whole = Whole Nat Sign

-- |The class of Peano-like constructions (i.e. Nat and Whole).
class Enum a => Peano a where
   -- |Test for zero.
   isZero :: a -> Bool
   -- |An unobservable infinity.
   infinity :: a
   -- |Converts the number to an Integer.
   fromPeano :: a -> Integer
   -- |Reduces the absolute value of the number by 1. If @isZero n@, then
   --  @decr n = n@.
   decr :: a -> a

-- |Negation of 'isZero'.
isSucc :: Peano n => n -> Bool
isSucc = not . isZero

instance Peano Nat where
   isZero Z = True
   isZero _ = False
   infinity = S infinity
   fromPeano Z = 0
   fromPeano (S n) = succ $ fromPeano n
   decr = pred

instance Peano Whole where
   isZero (Whole n _) = isZero n
   infinity = Whole infinity Pos
   fromPeano (Whole n Pos) = fromPeano n
   fromPeano (Whole n Neg) = negate $ fromPeano n
   decr (Whole n s) = Whole (pred n) s

-- |Removes at most 'S' constructors from a Peano number.
--  Outputs the number of removed constructors and the remaining number.
takeNat :: (Num a, Enum a, Ord a, Peano n) => a -> n -> (a, n)
takeNat = takeNat' 0
   where
      takeNat' c i n | i <= 0    = (c, n)
                     | isZero n  = (c, n)
                     | otherwise = takeNat' (succ c) (pred i) (decr n)


-- |Bounded-instance for 'Nat'. The lower bound is zero, the upper bound
--  is infinity.
instance Bounded Nat where
   minBound = Z
   maxBound = infinity

-- |Bounded-instance for 'Nat'. The bounds are negative and positive infinity.
instance Bounded Whole where
   minBound = Whole infinity Neg
   maxBound = infinity

-- |Enum-instance for 'Nat'. The 'pred' function is bounded at Zero.
instance Enum Nat where
   toEnum = fromInteger . fromIntegral
   fromEnum = fromInteger . fromPeano
   succ = S
   pred Z = Z
   pred (S n) = n

-- |Enum-instance for 'Whole'. 'succ' and 'pred' work according to the total
--  order on the whole numbers, i.e. @succ n = n+1@ and @pred n = n-1@.
instance Enum Whole where
   toEnum i | i < 0     = Whole (toEnum i) Neg
            | otherwise = Whole (toEnum i) Pos
   fromEnum = fromInteger . fromPeano
   succ (Whole (S n) Neg) = Whole n Neg
   succ (Whole n Pos) = Whole (S n) Pos
   succ (Whole Z _) = Whole (S Z) Pos
   pred (Whole (S n) Pos) = Whole n Pos
   pred (Whole n Neg) = Whole (S n) Neg
   pred (Whole Z _) = Whole (S Z) Neg

-- |Num-instance for 'Nat'. Addition, multiplication, and subtraction are
--  lazy in both arguments, meaning that, in the case of infinite values,
--  they can produce an infinite stream of S-constructors. As long as
--  the callers of these functions only consume a finite amount of these,
--  the program will not hang.
--
--  @fromInteger@ is not injective in case of 'Nat', since negative integers
--  are all converted to Z.
instance Num Nat where
   (+) Z n = n
   (+) n Z = n
   (+) (S n) (S m) = S $ S $ (+) n m

   (*) Z n = Z
   (*) n Z = Z
   (*) (S n) (S m) = S Z + n + m + (n * m)

   abs = id

   signum _ = S Z

   fromInteger i | i <= 0    = Z
                 | otherwise = S $ fromInteger $ i - 1

   (-) Z n = Z
   (-) n Z = n
   (-) (S n) (S m) = n - m

instance Num Whole where
   (+) (Whole n Pos) (Whole m Pos) = Whole (n+m) Pos
   (+) (Whole n Neg) (Whole m Neg) = Whole (n+m) Neg
   (+) (Whole n Pos) (Whole m Neg) | n >= m    = Whole (n-m) Pos
                                   | otherwise = Whole (m-n) Neg
   (+) (Whole n Neg) (Whole m Pos) = Whole m Pos + Whole n Neg

   (*) (Whole n s) (Whole m t) | s == t    = Whole (n*m) Pos
                               | otherwise = Whole (n*m) Neg

   (-) n (Whole m Neg) = n + (Whole m Pos)
   (-) n (Whole m Pos) = n + (Whole m Neg)

   abs (Whole n s) = Whole n Pos

   signum (Whole Z _) = Whole Z Pos
   signum (Whole _ Pos) = Whole (S Z) Pos
   signum (Whole _ Neg) = Whole (S Z) Neg

   fromInteger i | i < 0     = Whole (fromInteger $ negate i) Neg
                 | otherwise = Whole (fromInteger i) Pos


-- |Eq-instance for 'Nat'. '==' and '/=' work as long as at least one operand
--  is finite.
instance Eq Nat where
   (==) Z Z = True
   (==) Z (S _) = False
   (==) (S _) Z = False
   (==) (S n) (S m) = n == m

-- |Eq-instance for 'Whole'. Positive and negative zero are considered equal.
instance Eq Whole where
   (==) (Whole Z _) (Whole Z _) = True
   (==) (Whole n s) (Whole m t) = s == t && n == m

-- |Ord-instance for 'Nat'. All methods work as long as at least one operand
--  is finite.
instance Ord Nat where
   compare Z Z = EQ
   compare Z (S _) = LT
   compare (S _) Z = GT
   compare (S n) (S m) = compare n m

instance Ord Whole where
   compare (Whole Z _) (Whole Z _) = EQ
   compare (Whole _ Neg) (Whole _ Pos) = LT
   compare (Whole _ Pos) (Whole _ Neg) = GT
   compare (Whole n Pos) (Whole m Pos) = compare n m
   compare (Whole n Neg) (Whole m Neg) = compare m n

-- |Returns the length of a list as Nat. Can handle infinite lists, but
-- IS NOT tail-recursive, since an accumulator function would defeat the
-- laziness.
natLength :: [a] -> Nat
natLength [] = Z
natLength (_:xs) = S $ natLength xs

-- |Real-instance for 'Nat'. Since 'toRational' returns a @Ratio Integer@,
--  it WILL NOT terminate on infinities.
instance Real Nat where
   toRational = flip (%) 1 . fromPeano

-- |Integral-instance for 'Nat'. Since negative numbers are not allowed,
--  @quot = div@ and @rem = mod@. The methods @quot@, @rem@, @div@, @mod@,
--  @quotRem@ and @divMod@ will return as long as their first argument is
--  finite. Infinities are handled as follows:
--  @
--  n `quot` infinity   =   n `div` infinity   =   0
--  n `rem`  infinity   =   n `mod` infinity   =   n@
instance Integral Nat where
   quotRem _ Z = error "divide by zero"
   quotRem n (S m) = quotRem' Z n (S m)
      where
         quotRem' q n m | n >= m    = quotRem' (S q) (n-m) m
                        | otherwise = (q,n)

   divMod = quotRem
   toInteger = fromPeano
