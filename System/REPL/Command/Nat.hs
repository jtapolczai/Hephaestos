-- Minimal Peano arithmetic module.
module System.REPL.Command.Nat where

-- |Lazy Peano numbers. Addition, multiplication, and subtraction
--  are lazy in BOTH arguments.
--  Subtraction is bounded at 0, meaning that @n - m = Z@ if
--  @m >= n@. Furthermore, @negate n = Z@.
--  Methods of 'Eq' and Ord' will work if at least one argument is finite.
data Nat = Z | S Nat deriving (Show)

isZero :: Nat -> Bool
isZero Z = True
isZero _ = False

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

-- |Converts a number to Nat.
toNat :: (Num a, Ord a) => a -> Nat
toNat i | i <= 0    = Z
        | otherwise = S $ toNat $ i - 1

-- |Converts a finite to a number.
fromNat :: (Num a, Enum a) => Nat -> a
fromNat Z = 0
fromNat (S n) = succ $ fromNat n

instance Enum Nat where
   toEnum = toNat
   fromEnum = fromNat
   succ = S
   pred Z = Z
   pred (S n) = n

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

instance Eq Nat where
   (==) Z Z = True
   (==) Z (S _) = False
   (==) (S _) Z = False
   (==) (S n) (S m) = n == m

instance Ord Nat where
   (<=) Z Z = True
   (<=) Z (S _) = True
   (<=) (S _) Z = False
   (<=) (S n) (S m) = n <= m

-- |Returns the length of a list as Nat. Can handle infinite lists.
natLength :: [a] -> Nat
natLength [] = Z
natLength (_:xs) = succ $ natLength xs
