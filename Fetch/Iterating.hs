module Fetch.Iterating where

import Control.Monad
import Control.Monad.Loops
import Data.Maybe (catMaybes)
import Data.Text (unpack)
import Network.HTTP.Conduit

import Fetch
import XPath

type CrawlIterator = Maybe URL -> ErrorIO (Maybe (Maybe URL, Maybe URL))

fetchIterate :: Manager -> TextExtractor -> TextExtractor -> CrawlIterator
fetchIterate _ _ _ Nothing = return Nothing
fetchIterate m next item (Just url) = 
   do res <- download m url
      doc <- toDocument url res
      let nextLink = next doc
          itemLink = item doc
      return $! Just (liftM unpack itemLink, liftM unpack nextLink)

fetchList :: URL -> CrawlIterator -> ErrorIO [URL]
fetchList start iter =
   do list <- unfoldrM iter (Just start) 
      return $! catMaybes list
