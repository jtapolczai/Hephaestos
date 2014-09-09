{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for the Cyanide and Happiness webcomic.
module Comics.XKCD where

import Network.HTTP.Conduit
import Data.Text

import Fetch
import Fetch.Iterating
import Helper.String
import XPath

-- |The list of all Cyanide and Happiness comics, starting with a given one.
xkcdList :: Manager -> URL -> ErrorIO [URL]
xkcdList m from = fetchList from (fetchIterate m xkcdNext xkcdComic)

-- |The first Cyanide and Happiness comic.
xkcdStart :: URL
xkcdStart = "http://www.xkcd.com/1/"

dom :: Text
dom = "http://xkcd.com/"

-- |Gets the source of the comic image from a Cyanide and Happiness page.
xkcdComic :: TextExtractor
xkcdComic = mkNothing . concatText . getXPath "//div[@id=\"comic\"]/img/@src/text()"

-- |Gets the URL of the "next"-link from a Cyanide and Happiness page.
xkcdNext :: TextExtractor
xkcdNext = fmap (appendAbs dom) .  mkNothing . concatText . getXPath "(//a[@rel=\"next\"])[1]/@href/text()"
