module Fetch.Iterating where

import Control.Monad
import Control.Monad.Loops
import Data.Maybe (catMaybes)
import Data.Text (unpack, Text)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor

import Fetch

type CrawlIterator = (Manager, Maybe URL) -> IO (Maybe (Maybe URL, (Manager, Maybe URL)))
type TextExtractor = Cursor -> Maybe Text

fetchIterate :: TextExtractor -> TextExtractor -> CrawlIterator
fetchIterate _ _ (_,Nothing) = return Nothing
fetchIterate next item (m,Just path) = 
   do (m', f) <- withManager download (Just m) path
      case f of Left _ -> return Nothing
                Right f' ->
                   do let doc = parseLBS f'
                          nextLink = runXPath doc next
                          itemLink = runXPath doc item
                      return $! Just (liftM unpack itemLink, (m', liftM unpack nextLink))

fetchList :: URL -> CrawlIterator -> IO [URL]
fetchList start iter =
   do m <- newManager defaultManagerSettings
      list <- unfoldrM (status >=> iter) (m, Just start) 
      return $! catMaybes list
   where
      status (m,Nothing) = return (m,Nothing) 
      status (m,Just path) = putStrLn (path ++ " retrieved!") >> return (m,Just path)
