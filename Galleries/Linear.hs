{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for standard-issue linear webcomics
--  which present one image per page, together with
--  next/previous buttons. The functionality provided
--  herein should be enough to cover most webcomics.
module Galleries.Linear where

import Control.Arrow
import Network.HTTP.Conduit
import Data.Maybe
import Data.Text hiding (map)
import Data.Void

import Fetch
import Fetch.Tree
import Helper.String
import XPath

-- |Descriptor of a linear webcomic.
data LinearComic =
   LinearComic{comicName::Text, -- ^The comic's name.
               domain::URL, -- ^The domain name.
                            -- Will be prepended to relative links.
               firstComic::URL, -- ^The URL of the first comic.
               currentComic::URL, -- ^The URL of the most current comic.
               imgXPath::String, -- ^The XPath expression of the image.
                                 -- Must return text.
               nextXPath::String, -- ^The XPath expression of the "next" link.
                                  -- Must return text.
               prevXPath::String -- ^The XPath expression of the "previous" link.
                                 -- Must return text.
               }
   deriving (Show, Eq, Read)

-- |Gets all pages of a linear webcomic, going from the first to the
--  current comic.
getLinearComic :: Manager -> LinearComic -> ErrorIO [URL]
getLinearComic m lc = flattenTree $ fetchTree' m suc (firstComic lc)
   where
      suc = comicNext (imgXPath lc) (nextXPath lc)

-- |Gets n pages f a linear webcomic, going back from the current one.
getLinearComicBack :: Manager -> LinearComic -> Int -> ErrorIO [URL]
getLinearComicBack m lc i = flattenTree
                            $ fetchTree m suc (Just i) (currentComic lc)
   where
      suc = comicPrev (imgXPath lc) (prevXPath lc)


-- |Generic "next" functon for comics.
--  Returns a leaf (the comic) and a node (the URL of the next comic).
comicNext :: String -- ^XPath expression to the image's URL.
          -> String -- ^XPath expression to the next comic's URL.
          -> Successor Void
comicNext c n _ doc _ =
   (map unpack $ toList $ concatText $ getXPath c doc,
    map (unpack &&& const undefined) $ toList $ concatText $ getXPath n doc)

-- |Generic bounded "previous" function for comics.
--  If no counter is given, this is simple the reverse of 'comicNext'.
--  This functions works basically like 'comicNext', with the following
--  difference: if no counter is given, it is the reverse of 'comicNext'.
--  If a counter is given, it is decremented by 1 each time until it reaches
--  0. When that happens, the function returns @([],[],counter-1)@.
comicPrev :: String -> String -> Successor (Maybe Int)
comicPrev c p _ doc cnt | isNothing cnt     = (comic, prev)
                        | fromJust cnt <= 0 = ([],[])
                        | otherwise         = (comic, prev)
   where comic = map unpack $ toList $ concatText $ getXPath c doc
         prev = map (unpack &&& const (fmap (\x -> x-1) cnt))
                $ toList $ concatText $ getXPath p doc
