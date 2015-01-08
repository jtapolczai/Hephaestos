{-# LANGUAGE OverloadedStrings #-}

-- |Crawlers for porn sites.
module Crawling.Hephaestos.Crawlers.Test.Porn where

import Prelude hiding (concat, (++))
import qualified Prelude as P

import Control.Monad
import Control.Monad.Except
import Data.Functor.Monadic
import Data.Maybe (mapMaybe)
import Data.Text hiding (map, last, head, filter, null)
import Data.Void
import System.FilePath.Generic
import System.IO

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Helper.String
import Crawling.Hephaestos.XPath

freeonesTree :: Successor [NetworkError] Void
freeonesTree = htmlSuccessor id freeonesTree'
   where
      freeonesTree' url' doc _ = (images, nextPage)
            where
               url = dropFileName url'

               images =
                  mapMaybe (getText >=$> combineURL url >=$> Blob >=$> voidNode)
                  $ getXPathLeaves "//div[@id=\"PictureList\"]/a/@href/text()" doc

               nextPage =
                  map (combineURL url |> Inner |> voidNode)
                  $ getSingleText
                  $ getXPathLeaves "//*[@id=\"Viewport\"]//a[@class=\"gallery-nav gallery-nav-right ir active\"]/@href/text()" doc

freeones :: Monad m => VoidCrawler m
freeones = voidCrawler "freeones"
                       "http://www.freeones.com/*"
                       freeonesTree


motherlessTree :: Successor [NetworkError] Int
motherlessTree = htmlSuccessor id motherlessTree'
   where
      motherlessTree' url doc i | i < 0 = ([],[])
                                | otherwise = (image, images P.++ galleries P.++ nextPage)
         where
            image = map (simpleNode (i-i))
                    $ (if null galleries && null images
                        then noneAsDataFailure url else id)
                    $ map (combineURL "http://motherless.com/" |> Blob)
                    $ getSingleText
                    $ getXPathLeaves "//*[@id=\"media-media\"]/div/a/img/@src/text()" doc

            galleries = filter ((>=0).nodeState)
                        $ mapMaybe (getText >=$> combineURL "http://motherless.com/"
                                    >=$> Inner
                                    >=$> simpleNode (i-1))
                        $ getXPathLeaves (subitem "contains(text(), 'Files')") doc

            images = map (combineURL "http://www.motherless.com/" |> Inner |> simpleNode i)
                     $ mapMaybe getText
                     $ getXPathLeaves (subitem "contains(text(), 'x')") doc

            nextPage = map (combineURL "http://www.motherless.com/" |> Inner |> simpleNode i)
                       $ getSingleText
                       $ getXPathLeaves "//div[@id=\"content\"]/div[@class=\"browse\"]/div[@class=\"center\"]/div[@class=\"pagination_link\"]/a[contains(.,'NEXT')]/@href/text()" doc

      subitem crit = "//*[@id=\"content\"]//div[@class=\"content-inner\"]" ++
                     "//div[@data-is-premium=\"0\" and " ++
                     "descendant::div[@class=\"caption left\" and " ++
                     crit ++ "]]/a/@href/text()"


motherless :: (MonadIO m) => TreeCrawler m Void Int
motherless = stateCrawler "motherless"
                          "http://www.motherless.com/*"
                          motherlessTree
                          (liftIO $ liftM read (putStr "Gallery depth limit: " >> hFlush stdout >> getLine))

eightMusesTree :: Successor [NetworkError] Void
eightMusesTree = htmlSuccessor id eightMusesTree'
   where
      eightMusesTree' url doc _ = (title P.++ image, galleries)
         where
            galleries = mapMaybe (getText >=$> combineURL url >=$> Inner >=$> voidNode)
                        $ getXPathLeaves "//article/a/@href/text()" doc

            title = map (Info "title" |> voidNode)
                    $ getSingleText
                    $ getXPathLeaves "//*[id=\"main\"]/h1/a[last()]/text()" doc

            image = map (combineURL url |> Blob |> voidNode)
                    $ getSingleText
                    $ getXPathLeaves "//*[@class=\"imgLiquidFill\"]//img/@src/text()" doc

eightMuses :: Monad m => VoidCrawler m
eightMuses = voidCrawler "8Muses"
                         "http://8muses.com/*"
                         eightMusesTree
