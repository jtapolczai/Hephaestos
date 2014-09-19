-- |Simple galleries. This module is deprecated and will be removed soon.
module Galleries.Simple where

import Control.Arrow
import Data.Char
import Data.List.Split
import qualified Data.Text as T

import Fetch
import Helper.String hiding ((++))

-- |Takes a URL of the form \"X\<num\>Y\" where '<num>' is a
--  decimal integer and 'Y' contains no digits. That is, '<num>' is the
--  last number in the URL.
--  This function returns a list of URLs which correspond to the
--  input URL, '<num>' having been replaced by the integers in the given
--  range. If '<num>' has leading zeroes, all numbers will be padded
--  with zeroes to '<num>'\'s length.
--
-- Example:
--
-- >>> mapM_ putStrLn $ pictureList "http://domain.com/image001.jpg" [3..5]
-- http://domain.com/image003.jpg
-- http://domain.com/image004.jpg
-- http://domain.com/image005.jpg
pictureList :: URL -> [Int] -> [URL]
pictureList url range = map (\i -> T.pack $ b ++ i ++ a) indices
   where
      (b',e,a') = getLast (uncurry (&&) . ((not.null) &&& all isDigit))
                  $ split (whenElt (not.isDigit)) $ T.unpack url
      b = concat b'
      indices = map (padLeft '0' padLength . show) range
      a = concat a'

      padLength = maybe 0 length e
