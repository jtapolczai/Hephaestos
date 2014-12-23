-- |Helper functions for functors.
--
--  These operators are designed to make the interoperation between monadic
--  and pure computations more convenient by allowed them to be chained together
--  without peppering the program with superflouos return statements.
--
--  Each function is a pure analogue of a monadic one. The correspondences are
--  as follows:
--  @
--  >$>   ~  >>=
--  $>    ~  >>
--  <$    ~  <<   (re-exported from Data.Functor)
--  <$<   ~  =<<  (same as Data.Functor.<$>, but with the precedence of >>=)
--  >=$>  ~  >=>
--  <$=<  ~  <=<
--  @
--
-- Lastly, '|>' is left-to-right function composition (flipped version of '$').
module Data.Functor.Monadic (
   module Data.Functor,
   (>$>),
   ($>),
   (<$<),
   (>=$>),
   (<$=<),
   (|>)) where

import Data.Functor ((<$))

infixl 1 >$>
infixl 1 $>
infixr 1 <$<
infixl 1 >=$>
infixr 1 <$=<
infixl 9 |>

-- |Flipped 'fmap' for chaining plain functions after a functor in the following
--  way:
--
-- @
-- readFile '1.txt' >$> lines >$> map length >>= print
-- @
--
-- @lines@ and @map length@ are non-monadic functions, but peppering
-- them with returns, as pure '>>=' necessitates, is quite tedious.
--
-- In general:
--
-- @
-- m >>= return . f   is the same as   m >$> f
-- @
(>$>) :: Functor f => f a -> (a -> b) -> f b
(>$>) = flip fmap

-- |Left-associatiative, flipped '$>'. Corresponds to '>>'
($>) :: Functor f => f b -> a -> f a
($>) = flip (<$)

-- |Right-associative infix synonym for 'fmap'.
(<$<) :: Functor f => (a -> b) -> f a -> f b
(<$<) = fmap

-- |Application of '>$>' to Kleisli composition 'Control.Monad.>=>'
--  Use is analogous to that of '>$>', e.g.
--
--  @
--  f :: FilePath -> IO ()
--  f = (readFile >=$> lines >=$> map length >=> print)
--  @
--
--  In general:
--
--  @
--  m >=$> f   is the same as   m >=> return . f
--  @
(>=$>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(>=$>) f g x = f x >$> g

-- |Flipped version of '>=$>'.
(<$=<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$=<) = flip (>=$>)

-- |Flipped version of '$'.
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
