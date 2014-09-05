module Fetch.Iterating where

import Control.Monad
import Control.Monad.Loops
import Data.Maybe (catMaybes)
import Data.Text (unpack, Text)
import Network.HTTP.Conduit hiding (path, withManager)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

import Fetch

type CrawlIterator = Maybe URL -> ErrorIO (Maybe (Maybe URL, Maybe URL))
type TextExtractor = Cursor -> Maybe Text

fetchIterate :: Manager -> TextExtractor -> TextExtractor -> CrawlIterator
fetchIterate _ _ _ Nothing = return Nothing
fetchIterate m next item (Just path) = 
   do res <- download m path
      let doc = parseLBS res
          nextLink = runXPath doc next
          itemLink = runXPath doc item
      return $! Just (liftM unpack itemLink, liftM unpack nextLink)

fetchList :: URL -> CrawlIterator -> ErrorIO [URL]
fetchList start iter =
   do list <- unfoldrM iter (Just start) 
      return $! catMaybes list
