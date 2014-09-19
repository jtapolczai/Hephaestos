{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for standard-issue linear webcomics
--  which present one image per page, together with
--  next/previous buttons. The functionality provided
--  herein should be enough to cover most webcomics.
module Galleries.Linear where

import Network.HTTP.Conduit
import Data.Maybe
import Data.Text hiding (map)
import Data.Void

import Fetch
import Fetch.Tree
import Fetch.Types.Successor
import XPath

-- |Descriptor of a linear webcomic.
--  This type assumes that both the images and the links
--  can be retrieved through XPath expressions.
--  This assumptions probably holds for most webcomics, but
--  will fail for JavaScript-based and complexly structured ones.
data LinearCrawler =
   LinearComic{comicName::Text, -- ^The comic's name.
               domain::URL, -- ^The domain name.
                            -- Will be prepended to relative links.
               firstComic::URL, -- ^The URL of the first comic.
               currentComic::URL, -- ^The URL of the most current comic.
               imgXPath::Text, -- ^The XPath expression of the image.
                               -- Must return text.
               nextXPath::Text, -- ^The XPath expression of the "next" link.
                                -- Must return text.
               prevXPath::Text -- ^The XPath expression of the "previous" link.
                               -- Must return text.
               }
   deriving (Show, Eq, Read)

-- |Gets all pages of a linear webcomic, going from the first to the
--  current comic.
getLinearComic :: Manager -> LinearCrawler -> ErrorIO [URL]
getLinearComic m lc = extractBlobs $ fetchTree' m suc (firstComic lc)
   where
      suc = comicNext (imgXPath lc) (nextXPath lc)

-- |Gets n pages f a linear webcomic, going back from the current one.
getLinearComicBack :: Manager -> LinearCrawler -> Int -> ErrorIO [URL]
getLinearComicBack m lc i = extractBlobs
                            $ fetchTree m suc (Just i) (currentComic lc)
   where
      suc = comicPrev (imgXPath lc) (prevXPath lc)


-- |Generic "next" functon for comics.
--  Returns a leaf (the comic) and a node (the URL of the next comic).
comicNext :: Text -- ^XPath expression to the image's URL.
          -> Text -- ^XPath expression to the next comic's URL.
          -> Successor Void NetworkError
comicNext c n _ doc _ =
   (map Blob $ mapMaybe getText $ getXPathLeaves c doc,
    mapState undefined $ map Blob $ getSingleText $ getXPathLeaves n doc)

-- |Generic bounded "previous" function for comics.
--  If no counter is given, this is simple the reverse of 'comicNext'.
--  This functions works basically like 'comicNext', with the following
--  difference: if no counter is given, it is the reverse of 'comicNext'.
--  If a counter is given, it is decremented by 1 each time until it reaches
--  0. When that happens, the function returns @([],[],counter-1)@.
comicPrev :: Text -> Text -> Successor (Maybe Int) NetworkError
comicPrev c p _ doc cnt | isNothing cnt     = (comic, prev)
                        | fromJust cnt <= 0 = ([],[])
                        | otherwise         = (comic, prev)
   where comic = map Blob $ mapMaybe getText $ getXPathLeaves c doc
         prev = mapState cnt' $ map Blob $ getSingleText $ getXPathLeaves p doc
         cnt' = fmap (\x -> x - 1) cnt
