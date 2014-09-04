module Fetch.Mapping where

import Fetch

-- |Puts a prefix and a suffix around every element
--  of a list. Useful for creating lists of
--  URLs.
pictureFetch :: String -> String -> [String] -> [URL]
pictureFetch before after = map (splice before after)

-- |Concatenates three lists. The third list is put between the
--  first two.
splice :: [a] -> [a] -> [a] -> [a]
splice b a s = b ++ s ++ a