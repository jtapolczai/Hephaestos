{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |Specific crawlers which have, at the core, 'Successor' functions,
--  but add extra features.
module Crawling.Hephaestos.Crawlers (
   SimpleLinearCrawler(..),
   CrawlerDirection(..),
   crawlerNext,
   crawlerPrev,
   ) where

import Prelude hiding (succ)

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Functor.Monadic
import Data.Maybe
import Data.Monoid (mempty)
import qualified Data.Text.Lazy as TL
import Data.Void
import qualified Network.URI as N

import Crawling.Hephaestos.Crawlers.Utils
--import qualified Crawling.Hephaestos.Fetch.Types as FT()
import Crawling.Hephaestos.Fetch.Successor


-- |Descriptor of a linear crawler which extracts content
--  and a 'next'-link from each URL via XPath expressions.
--  @a@ is a phantom type, added to satisfy the @*->*@ kind
--  expected by the 'Crawler' typeclass.
data SimpleLinearCrawler =
   SimpleLinearCrawler{slcName::TL.Text, -- ^The crawler's name.
                       slcDescription::TL.Text, -- ^The crawler's description.
                       slcDomain::N.URI,
                       -- ^The domain name. Will be prepended to relative links.
                       slcFirstURL::N.URI, -- ^The URL of the first comic.
                       slcLastURL::N.URI, -- ^The URL of the most current comic.
                       slcContentXPath::TL.Text,
                       -- ^The XPath expression of the image. Must return text.
                       slcNextXPath::TL.Text,
                       -- ^The XPath expression of the "next" link. Must return text.
                       slcPrevXPath::TL.Text
                       -- ^The XPath expression of the "previous" link.
                       --Must return text.
                       }
   deriving (Show, Eq)

instance ToJSON SimpleLinearCrawler where
   toJSON cr = object ["type" .= ("SimpleLinearCrawler"::TL.Text),
                       "name" .= slcName cr,
                       "description" .= slcDescription cr,
                       "domain" .= show (slcDomain cr),
                       "firstURL" .= show (slcFirstURL cr),
                       "lastURL" .= show (slcLastURL cr),
                       "contentXPath" .= slcContentXPath cr,
                       "nextXPath" .= slcNextXPath cr,
                       "prevXPath" .= slcPrevXPath cr]

instance FromJSON SimpleLinearCrawler where
   parseJSON (Object v) = do
      typ <- v .: "type"
      if typ /= ("SimpleLinearCrawler" :: TL.Text) then mzero
      else do name <- v .: "name"
              description <- v .: "description"
              domain <- (v .: "domain") >$> N.parseURIReference
              firstURL <- (v .: "firstURL") >$> N.parseURIReference
              lastURL <- (v .: "lastURL") >$> N.parseURIReference
              contentXPath <- v .: "contentXPath"
              nextXPath <- v .: "nextXPath"
              prevXPath <- v .: "prevXPath"
              case sequence [domain, firstURL, lastURL] of
                 Just [d,f,l] -> return $
                    SimpleLinearCrawler name description d f l
                                        contentXPath nextXPath prevXPath
                 _ -> mzero
   parseJSON _ = mzero


-- |Indicates the direction of a 'SimpleLinearCrawler'.
data CrawlerDirection = Backwards | Forwards deriving (Show, Eq, Read)

-- Crawler logic
-------------------------------------------------------------------------------

-- |Gets the successor function of a SimpleLinearCrawler.
crawlerNext :: SimpleLinearCrawler
            -> CrawlerDirection -- ^Direction.
            -> Successor SomeException Void (Maybe Int)
crawlerNext SimpleLinearCrawler{slcContentXPath=content,
                                slcNextXPath   =next,
                                slcPrevXPath   =prev} =
   \case Forwards  -> simpleLinearSucc content next
         Backwards -> simpleLinearSucc content prev

-- |Gets the successor function of a SimpleLinearCrawler, but goes in the opposite
--  direction of 'crawlerNext'.
crawlerPrev :: SimpleLinearCrawler
            -> CrawlerDirection
            -> Successor SomeException Void (Maybe Int)
crawlerPrev c = crawlerNext c . invert
      where
         invert Forwards = Backwards
         invert Backwards = Forwards

simpleLinearSucc ::TL.Text -> TL.Text -> Successor SomeException Void (Maybe Int)
simpleLinearSucc xpContent xpLink = htmlSuccessor mempty
                                        $ simpleLinearSucc' xpContent xpLink

simpleLinearSucc' :: TL.Text -> TL.Text -> HTMLSuccessor SomeException Void (Maybe Int)
simpleLinearSucc' xpContent xpLink uri doc counter
   | isNothing counter || fromJust counter > 0 = return $ content ++ link
   | otherwise = return []
   where
      content = map (SuccessorNode counter' . makeLink uri (Blob Nothing))
                $ mapMaybe getText $ getXPath xpContent doc

      link = map (SuccessorNode counter' . makeLink uri Inner)
             $ getSingleText
             $ getXPath xpLink doc

      counter' = fmap (subtract (length content)) counter
