{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for linear webcomics.
module Comics.List where

import Network.HTTP.Conduit
import Data.Text

import Comics.LinearComic
import Fetch
import Fetch.Iterating
import Helper.String
import XPath

xkcd =
   LinearComic "XKCD"
               "http://www.xkcd.com/"
               "http://www.xkcd.com/1/"
               "http://www.xkcd.com"
               "//div[@id=\"comic\"]/img/@src/text()"
               "(//a[@rel=\"next\"])[1]/@href/text()"
               "(//a[@rel=\"prev\"])[1]/@href/text()"

pennyArcade =
   LinearComic "Penny Arcade"
               "http://www.penny-arcade.com/"
               "http://www.penny-arcade.com/comic/1998/11/18"
               "http://www.penny-arcade.com/comic"
               "//*[@id=\"comicFrame\"]//img/@src/text()"
               "(//a[@class=\"btn btnNext\"])[1]/@href/text()"
               "(//a[@class=\"btn btnPrev\"])[1]/@href/text()"

cyanideAndHappiness =
   LinearComic "Cyanide and Happiness"
               "http://www.explosm.net/"
               "http://explosm.net/comics/15/"
               "http://explosm.net/comics/"
               "//*[@id=\"maincontent\"]/div[2]/div[1]/img/@src/text()"
               "//a[@rel=\"next\"]/@href/text()"
               "//a[@rel=\"prev\"]/@href/text()"
