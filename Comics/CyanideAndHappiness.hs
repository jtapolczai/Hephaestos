{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for the Cyanide and Happiness webcomic.
module Comics.CyanideAndHappiness where

import Data.Text
import Network.HTTP.Conduit

import Fetch
import Fetch.Iterating
import Helper.String
import XPath

-- |The list of all Cyanide and Happiness comics.
cyanideList :: Manager -> ErrorIO [URL]
cyanideList m = fetchList "http://explosm.net/comics/15"
                (fetchIterate m cyanideNext cyanideComic)

-- |Gets the source of the comic image from a Cyanide and Happiness page.
cyanideComic :: TextExtractor
cyanideComic = mkNothing . concatText . getXPath "//*[@id=\"maincontent\"]/div[2]/div[1]/img/@src/text()"

-- |Gets the URL of the "next"-link from a Cyanide and Happiness page.
cyanideNext :: TextExtractor
cyanideNext = mkNothing . append "http://explosm.net" . concatText . getXPath "//a[@rel=\"next\"]/@href/text()"
