-- |Helper functions for functors.
module Crawling.Hephaestos.Helper.Functor where

infixl 4 >$>

-- |Flipped 'fmap' for chaining plain functions after a functor in the following
--  way:
--
-- @
-- readFile '1.txt' >$> lines >$> map length >>= print
-- @
--
-- @lines@ and @map length@ are non-monadic functions, but peppering
-- them with returns, as pure '>>=' necessitates, is quite tedious.
(>$>) :: Functor f => f a -> (a -> b) -> f b
(>$>) = flip fmap

-- |Application of '>$>' to Kleisli composition 'Control.Monad.>=>'
--  Use is analogous to that of '>$>', e.g.
--
--  @
--  f :: FilePath -> IO ()
--  f = (readFile >=$> lines >=$> map length >=> print)
--  @
(>=$>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(>=$>) f g x = f x >$> g
