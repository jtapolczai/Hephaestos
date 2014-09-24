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
