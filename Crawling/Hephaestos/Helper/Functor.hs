-- |Helper functions for functors.
module Crawling.Hephaestos.Helper.Functor where

infixl 1 >$>
infixr 1 <$<
infixr 1 >=$>
infixr 1 <$=<

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
