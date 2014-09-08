{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for the Cyanide and Happiness webcomic.
module Comics.CyanideAndHappiness where

import Data.Text
import Network.HTTP.Conduit

import Fetch
import Fetch.Iterating
import Helper.String
import XPath

-- |The list of all Cyanide and Happiness comics, starting with a given one.
cyanideList :: Manager -> URL -> ErrorIO [URL]
cyanideList m from = fetchList from (fetchIterate m cyanideNext cyanideComic)

-- |The first Cyanide and Happiness comic.
cyanideStart :: URL
cyanideStart = "http://explosm.net/comics/15"

-- |Gets the source of the comic image from a Cyanide and Happiness page.
cyanideComic :: TextExtractor
cyanideComic = mkNothing . concatText . getXPath "//*[@id=\"maincontent\"]/div[2]/div[1]/img/@src/text()"

-- |Gets the URL of the "next"-link from a Cyanide and Happiness page.
cyanideNext :: TextExtractor
cyanideNext = mkNothing . append "http://explosm.net" . concatText . getXPath "//a[@rel=\"next\"]/@href/text()"
