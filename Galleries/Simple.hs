module Galleries.Simple where

import Control.Arrow
import Data.Char
import Data.List.Split

import Fetch

-- |Takes a URL of the form "X<num>Y" where '<num>' is a
--  decimal integer and 'Y' contains no digits. That is, '<num>' is the
--  last number in the URL.
--  This function returns a list of URLs which correspond to the
--  input URL, '<num>' having been replaced by the integers in the given
--  range. If '<num>' has leading zeroes, all numbers will be padded
--  with zeroes to '<num>'\'s length.
--
-- Example:
-- >>> mapM_ putStrLn $ pictureList "http://domain.com/image001.jpg" [3..5]
-- http://domain.com/image003.jpg
-- http://domain.com/image004.jpg
-- http://domain.com/image005.jpg
pictureList :: URL -> [Int] -> [URL]
pictureList url range = map (\i -> b ++ i ++ a) indices
   where
      (b',e,a') = getLast (uncurry (&&) . ((not.null) &&& all isDigit))
                  $ split (whenElt (not.isDigit)) url
      b = concat b'
      indices = map (padLeft '0' padLength . show) range
      a = concat a'

      padLength = maybe 0 length e

-- |Gets the last element of a list which fulfils a given predicate.
--  The elements of the list before and after that element are also
--  returned. Only works for finite lists.
--  @any f xs == True@ implies @getLast f xs == (ys,Just e,zs)@
--  such that @xs == ys ++ [e] ++ zs@, @f e == True@ and @any f zs == False@.
--  On the other hand, @any f xs == False@ implies
--  @getLast f xs == ([],Nothing,xs)@.
getLast :: (a -> Bool) -> [a] -> ([a],Maybe a,[a])
getLast f xs = (init' before, lastToMaybe before, after xs)
   where
      after = reverse . takeWhile (not . f) . reverse
      before = take (length xs - length (after xs)) xs

      lastToMaybe [] = Nothing
      lastToMaybe ys = Just $ last ys

      init' [] = []
      init' ys = init ys
