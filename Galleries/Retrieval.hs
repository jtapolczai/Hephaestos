module Galleries.Retrieval where

import Control.Arrow
import Data.Char
import Data.Either
import Data.List.Split
import Data.Text (unpack, Text)
import Network.HTTP.Conduit

import Fetch
import Fetch.Mapping
import Helper.String
import XPath

-- |Takes three extractor functions and a URL, and downloads a gallery of items.
--  The first one extracts a list of links to follow from the URL.
--  The second runs a list of functions on a single link.
--  each function should return a URL, which is then followed.
--  The content of this new URL is then given to the next function
--  in the list, until no more functions remain.
--  If any of the functions in the list fail, an error is added
--  (but 'galleryList' continues).
--
--  The third 
galleryList :: (Functor f)
            => Manager -- ^The (global) manager.
            -> (XmlTree -> [URL]) -- ^The target extractor which gets the list of targets to follow.
            -> [TextExtractor] -- ^The list of functions which are run on each target.
            -> f InfoExtractor -- ^The collection of functions which get local, auxiliary information from the page.
            -> URL -- ^The page which should be crawled.
            -> ErrorIO (f (Text, Maybe Text), [URL]) -- ^The auxiliary information, and the list of retrieved targets.
galleryList m targets paths auxInfo u =
   do res <- download m u
      doc <- toDocument u res
      let links = targets doc
          paths' = map (\p -> fmap unpack . p) paths
          aux = fmap ($ doc) auxInfo
      -- Fetch what's possible and collect the errors.
      imgs <- mapErr (flip (urlFetch m) paths') links
      -- Print error messages...
      errs <- collectErrors imgs
      mapM_ printError errs
      -- and return what was successful
      return (aux, rights imgs)

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
