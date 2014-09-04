{-# LANGUAGE OverloadedStrings #-}

module Comics.CyanideAndHappiness where

import Prelude hiding (concat)

import Data.Maybe (listToMaybe)
import Data.Text
import Text.XML.Cursor

import Fetch
import Fetch.Iterating

cyanideList :: IO [URL]
cyanideList = fetchList "http://explosm.net/comics/15"
              (fetchIterate cyanideNext cyanideComic)

cyanideComic :: TextExtractor
cyanideComic r =
   listToMaybe $ r $// attributeIs "id" "maincontent"
                    &/ element "div" >=> at 1
                    &/ element "div" >=> at 0
                    &/ element "img"
                    &| (concat . attribute "src")

cyanideNext :: TextExtractor
cyanideNext r =
   listToMaybe $ r $// orSelf (element "a")
                    &/ attributeIs "rel" "next"
                    &| (concat . ("http://explosm.net":) . attribute "href")
