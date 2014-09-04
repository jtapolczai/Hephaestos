module Fetch.Mapping where

import Fetch

pictureFetch :: String -> String -> [String] -> [URL]
pictureFetch before after = map (\i -> before++i++after)
