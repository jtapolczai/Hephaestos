{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


-- |Specific crawlers which have, at the core, 'Successor' functions,
--  but add extra features.
module Crawling.Hephaestos.Crawlers (
   -- *Classes
   Crawler(..),
   LinearCrawler(..),
   -- * Instances
   TreeCrawler(..),
   SimpleLinearCrawler,
   -- ** Helper functions and types
   VoidCrawler,
   voidCrawler,
   stateCrawler,
   ) where

import Prelude hiding (succ)

import Data.Maybe
import Data.Text hiding (map)
import Data.Void

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.XPath

-- |Descriptor for a general tree crawler which may or may not have a state @a@.
data TreeCrawler a =
   TreeCrawler Text -- ^The crawler's name.
               WildcardURL -- ^The crawler's domain.
               (IO a) -- ^A function which supplies the inital state.
               (Successor a NetworkError) -- ^The crawler's successor function.

-- |A TreeCrawler without a state.
type VoidCrawler = TreeCrawler Void

-- |Descriptor of a linear crawler which extracts content
--  and a 'next'-link from each URL via XPath expressions.
--  @a@ is a phantom type, added to satisfy the @*->*@ kind
--  expected by the 'Crawler' typeclass.
data SimpleLinearCrawler a =
   SimpleLinearCrawler{slcName::Text, -- ^The comic's name.
                       slcDomain::URL, -- ^The domain name.
                                       -- Will be prepended to relative links.
                       slcFirstURL::URL, -- ^The URL of the first comic.
                       slcLastURL::URL, -- ^The URL of the most current comic.
                       slcContentXPath::Text, -- ^The XPath expression of the image.
                                       -- Must return text.
                       slcNextXPath::Text, -- ^The XPath expression of the "next" link.
                                        -- Must return text.
                       slcPrevXPath::Text -- ^The XPath expression of the "previous" link.
                                          -- Must return text.
                       }
   deriving (Show, Eq, Read)


-- |The class of runnable crawlers.
class Crawler c a where
   -- |The name of a crawler.
   crawlerName :: c a -> Text
   -- |The domain of a crawler, if any. This is just for
   --  informative puproses.
   crawlerDomain :: c a -> WildcardURL
   -- |The 'Successor' function of the crawler.
   crawlerFunction :: c a -> Successor a NetworkError

-- |The class of crawlers with a linear structure, i.e.
--  one whose 'Successor' functions generate at most one
--  successor node.
--
--  LinearCrawlers can go forwards and backwards. 'crawlerFunction'
--  will be assumed to go forwards, whereas its inverse, 'prevFunc',
--  is assumed to go backwards in the list of URLs which the
--  crawler traverses.
class Crawler c a => LinearCrawler c a where
   -- |Gets the URL of the first (earliest) item.
   firstURL :: c a -> URL
   -- |Gets the URL of the last (latest) item.
   lastURL :: c a -> URL
   -- |The the inverse of 'crawlerFunction' which goes backwards
   --  in the list of visited URLs.
   prevFunc :: c a -> Successor a NetworkError


-- Instances
--------------------------------------------------------------------------------

instance Crawler TreeCrawler a where
   crawlerName (TreeCrawler n _ _ _) = n
   crawlerDomain (TreeCrawler _ d _ _) = d
   crawlerFunction (TreeCrawler _ _ _ f) = f


simpleLinearSucc :: Text -> Text -> Successor (Maybe Int) NetworkError
simpleLinearSucc xpContent xpLink _ doc counter
   | isNothing counter || fromJust counter > 0 = (content, link)
   | otherwise = ([],[])
   where
      content :: [FetchResult (Maybe Int) NetworkError]
      content = map Blob $ mapMaybe getText $ getXPathLeaves xpContent doc
      link = mapState counter'
             $ map Blob
             $ getSingleText
             $ getXPathLeaves xpLink doc
      counter' = fmap (\x -> x - 1) counter

instance Crawler SimpleLinearCrawler (Maybe Int) where
   crawlerName (SimpleLinearCrawler n _ _ _ _ _ _) = n
   crawlerDomain (SimpleLinearCrawler _ d _ _ _ _ _) = d
   crawlerFunction (SimpleLinearCrawler _ _ _ _ content next _) =
      simpleLinearSucc content next

instance LinearCrawler SimpleLinearCrawler (Maybe Int) where
   firstURL (SimpleLinearCrawler _ _ f _ _ _ _) = f
   lastURL (SimpleLinearCrawler _ _ _ l _ _ _) = l
   prevFunc (SimpleLinearCrawler _ _ _ _ content _ prev) =
      simpleLinearSucc content prev


-- Helper functions
--------------------------------------------------------------------------------


-- |Constructs a 'TreeCrawler' without state.
voidCrawler :: Text -> URL -> Successor Void NetworkError -> TreeCrawler Void
voidCrawler name dom succ = TreeCrawler name
                                        dom
                                        (return undefined)
                                        succ

-- |Constructs a 'TreeCrawler' with a state.
stateCrawler :: Text -> URL -> Successor a NetworkError -> IO a -> TreeCrawler a
stateCrawler name dom succ st = TreeCrawler name
                                            dom
                                            st
                                            succ
