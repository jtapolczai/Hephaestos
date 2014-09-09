{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for the Cyanide and Happiness webcomic.
module Comics.PennyArcade where

import Data.Text
import Network.HTTP.Conduit

import Fetch
import Fetch.Iterating
import Helper.String
import XPath

-- |The list of all Cyanide and Happiness comics, starting with a given one.
pennyArcadeList :: Manager -> URL -> ErrorIO [URL]
pennyArcadeList m from = fetchList from (fetchIterate m pennyArcadeNext pennyArcadeComic)

-- |The first Cyanide and Happiness comic.
pennyArcadeStart :: URL
pennyArcadeStart = "http://www.penny-arcade.com/comic/1998/11/18"

-- |Gets the source of the comic image from a Cyanide and Happiness page.
pennyArcadeComic :: TextExtractor
pennyArcadeComic = mkNothing . concatText . getXPath "//*[@id=\"comicFrame\"]//img/@src/text()"

-- |Gets the URL of the "next"-link from a Cyanide and Happiness page.
pennyArcadeNext :: TextExtractor
pennyArcadeNext = mkNothing . concatText . getXPath "(//a[@class=\"btn btnNext\"])[1]/@href/text()"
