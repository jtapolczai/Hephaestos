module Fetch.Mapping where

import Control.Monad
import Network.HTTP.Conduit hiding (path, withManager)
import Text.HTML.DOM (parseLBS)
import Text.XML (Document)

import Fetch

-- |Puts a prefix and a suffix around every element
--  of a list. Useful for creating lists of
--  URLs.
pictureFetch :: String -> String -> [String] -> [URL]
pictureFetch before after = map (splice before after)

-- |Executes a series of fetch requests.
--  At each step, a URL is retrieved and run against
--  the next function in the list. The result of the
--  last function is returned. In case of error, an exception is thrown.
urlFetch :: Manager -> URL -> [Document -> Maybe URL] -> ErrorIO URL
urlFetch m = foldM f
   where f :: URL -> (Document -> Maybe URL) -> ErrorIO URL
         f url g = do res <- download m url
                      case g $ parseLBS res of
                         Nothing -> addError $ DataFindingError $ "Could not find element on '"++url++"'!"
                         (Just r) -> return r

-- |Concatenates three lists. The third list is put between the
--  first two.
splice :: [a] -> [a] -> [a] -> [a]
splice b a s = b ++ s ++ a