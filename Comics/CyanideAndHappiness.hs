{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for the Cyanide and Happiness webcomic.
module Comics.CyanideAndHappiness where

import Prelude hiding (concat)

import Data.Maybe (listToMaybe)
import Data.Text
import Text.XML.Cursor

import Fetch
import Fetch.Iterating

-- |The list of all Cyanide and Happiness comics.
cyanideList :: IO [URL]
cyanideList = fetchList "http://explosm.net/comics/15"
              (fetchIterate cyanideNext cyanideComic)

-- |Gets the source of the comic image from a Cyanide and Happiness page.
cyanideComic :: TextExtractor
cyanideComic r =
   listToMaybe $ r $// attributeIs "id" "maincontent"
                    &/ element "div" >=> at 1
                    &/ element "div" >=> at 0
                    &/ element "img"
                    &| (concat . attribute "src")

-- |Gets the URL of the "next"-link from a Cyanide and Happiness page.
cyanideNext :: TextExtractor
cyanideNext r =
   listToMaybe $ r $// orSelf (element "a")
                    &/ attributeIs "rel" "next"
                    &| (concat . ("http://explosm.net":) . attribute "href")
