{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}

-- |Specific crawlers which have, at the core, 'Successor' functions,
--  but add extra features.
module Crawling.Hephaestos.Crawlers (
   SimpleLinearCrawler(..),
   CrawlerDirection(..),
   crawlerNext,
   crawlerPrev,
   crawlerState,
   crawlerConfig,
   ) where

import Prelude hiding (succ)

import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson
import Data.Dynamic
import Data.Functor.Monadic
import Data.Maybe
import Data.Text.Lazy hiding (map)
import Data.Void
import qualified Network.URI as N
import System.REPL

import Crawling.Hephaestos.Crawlers.Utils
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor

import qualified Data.Text as T

import Debug.Trace

-- |Descriptor of a linear crawler which extracts content
--  and a 'next'-link from each URL via XPath expressions.
--  @a@ is a phantom type, added to satisfy the @*->*@ kind
--  expected by the 'Crawler' typeclass.
data SimpleLinearCrawler =
   SimpleLinearCrawler{slcName::Text, -- ^The crawler's name.
                       slcDescription::Text, -- ^The crawler's description.
                       slcDomain::N.URI,
                       -- ^The domain name. Will be prepended to relative links.
                       slcFirstURL::N.URI, -- ^The URL of the first comic.
                       slcLastURL::N.URI, -- ^The URL of the most current comic.
                       slcContentXPath::Text,
                       -- ^The XPath expression of the image. Must return text.
                       slcNextXPath::Text,
                       -- ^The XPath expression of the "next" link. Must return text.
                       slcPrevXPath::Text
                       -- ^The XPath expression of the "previous" link.
                       --Must return text.
                       }
   deriving (Show, Eq)

instance ToJSON SimpleLinearCrawler where
   toJSON cr = object ["type" .= ("SimpleLinearCrawler"::Text),
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
      if typ /= ("SimpleLinearCrawler" :: T.Text) then mzero
      else do name <- v .: "name"
              description <- v .: "description"
              domain <- (v .: "domain") >$> N.parseURIReference
              firstURL <- (v .: "firstURL") >$> N.parseURIReference
              lastURL <- (v .: "lastURL") >$> N.parseURIReference
              contentXPath <- v .: "contentXPath"
              nextXPath <- v .: "nextXPath"
              prevXPath <- v .: "prevXPath"
              case sequence [domain, firstURL, lastURL] of
                 Nothing -> mzero
                 Just [d,f,l] -> return $
                    SimpleLinearCrawler name description d f l
                                        contentXPath nextXPath prevXPath
   parseJSON _ = mzero


-- |Indicates the direction of a 'SimpleLinearCrawler'.
data CrawlerDirection = Backwards | Forwards deriving (Show, Eq, Read, Ord, Enum)

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

-- |Asks the user for a number of items to crawl.
crawlerState :: (Functor m, MonadIO m, MonadCatch m)
             => m (Maybe Int)
crawlerState = ask' stateAsker >$> maybe Nothing Just
   where
      stateAsker = maybeAsker "Enter max. number of pages to get (or leave blank): "
                              "Expected positive integer!"
                              "Expected positive integer!"
                              (return . (>= 0))

-- |Asks the user for a crawler direction.
crawlerConfig :: (Functor m, MonadIO m, MonadCatch m)
              => m CrawlerDirection
crawlerConfig = ask' configAsker >$> fromMaybe Forwards
   where
      configAsker = maybeAsker "Enter direction (Forwards/Backwards; default=Forwards): "
                               "Expected Forwards/Backwards."
                               undefined
                               (const $ return True)

simpleLinearSucc :: Text -> Text -> Successor SomeException Void (Maybe Int)
simpleLinearSucc xpContent xpLink = htmlSuccessor id
                                    $ simpleLinearSucc' xpContent xpLink

simpleLinearSucc' :: Text -> Text -> HTMLSuccessor SomeException Void (Maybe Int)
simpleLinearSucc' xpContent xpLink uri doc counter
   | isNothing counter || fromJust counter > 0 = content ++ link
   | otherwise = []
   where
      content = map (SuccessorNode counter' . makeLink uri (Blob Nothing))
                $ mapMaybe getText $ getXPath xpContent doc

      link = map (SuccessorNode counter' . makeLink uri Inner)
             $ getSingleText
             $ getXPath xpLink doc

      counter' = fmap (subtract 1) counter

      err = error "simpleLinearSucc': Tried to access identifier!"
