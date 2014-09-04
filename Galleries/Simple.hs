module Galleries.Simple where

import Control.Arrow
import Data.Char
import Data.List.Split

import Fetch

pictureList :: URL -> [Int] -> [URL]
pictureList url range = map (\i -> b ++ i ++ a) indices
   where
      (b',e,a') = getLast (uncurry (&&) . ((not.null) &&& all isDigit))
                  $ split (whenElt (not.isDigit)) url
      b = concat b'
      indices = map (padLeft '0' padLength . show) range
      a = concat a'

      padLength = maybe 0 length e

getLast :: (a -> Bool) -> [a] -> ([a],Maybe a,[a])
getLast f xs = (init' before, lastToMaybe before, after xs)
   where
      after = reverse . takeWhile (not . f) . reverse
      before = take (length xs - length (after xs)) xs

      lastToMaybe [] = Nothing
      lastToMaybe ys = Just $ last ys

      init' [] = []
      init' ys = init ys
