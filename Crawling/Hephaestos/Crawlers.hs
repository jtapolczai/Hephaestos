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
   -- *Classes
   Crawler(..),
   LinearCrawler(..),
   StateCrawler(..),
   ConfigurableCrawler(..),
   -- * Instances
   TreeCrawler(..),
   SimpleLinearCrawler,
   CrawlerDirection(..),
   CrawlerDirection,
   -- ** Helper functions and types
   VoidCrawler,
   voidCrawler,
   stateCrawler,
   configCrawler,
   -- * Packing crawlers
   packCrawler,
   packCrawler',
   ) where

import Prelude hiding (succ)

import Control.Exception
import Control.Monad.Except
import Data.Dynamic
import Data.Functor.Monadic
import Data.HList.HList
import Data.Maybe
import Data.Text.Lazy hiding (map)
import Data.Void
import System.REPL

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.XPath

-- |Descriptor for a general tree crawler which may or may not have a state @a@.
data TreeCrawler m b a =
   TreeCrawler{tcName::Text, -- ^The crawler's name.
               tcDomain:: WildcardURL, -- ^The crawler's domain.
               tcConfig:: m b, -- ^A function which supples configuration data.
               tcInit::m a, -- ^A function which supplies the inital state.
               tcSucc:: b -> Successor SomeException a -- ^The crawler's successor function.
              }

-- |A TreeCrawler without a state or configuration.
type VoidCrawler m = TreeCrawler m Void Void

-- |Descriptor of a linear crawler which extracts content
--  and a 'next'-link from each URL via XPath expressions.
--  @a@ is a phantom type, added to satisfy the @*->*@ kind
--  expected by the 'Crawler' typeclass.
data SimpleLinearCrawler (m :: * -> *) b a =
   SimpleLinearCrawler{slcName::Text, -- ^The comic's name.
                       slcDomain::URL,
                       -- ^The domain name. Will be prepended to relative links.
                       slcFirstURL::URL, -- ^The URL of the first comic.
                       slcLastURL::URL, -- ^The URL of the most current comic.
                       slcContentXPath::Text,
                       -- ^The XPath expression of the image. Must return text.
                       slcNextXPath::Text,
                       -- ^The XPath expression of the "next" link. Must return text.
                       slcPrevXPath::Text
                       -- ^The XPath expression of the "previous" link.
                       --Must return text.
                       }
   deriving (Show, Eq, Read)

-- |Indicates the direction of a 'SimpleLinearCrawler'.
data CrawlerDirection = Backwards | Forwards deriving (Show, Eq, Read, Ord, Enum)


-- Classes
--------------------------------------------------------------------------------
-- |The class of runnable crawlers.
class Crawler c b a where
   -- |The name of a crawler.
   crawlerName :: c b a -> Text
   -- |The domain of a crawler, if any. This is just for
   --  informative puproses.
   crawlerDomain :: c b a -> WildcardURL
   -- |The 'Successor' function of the crawler.
   crawlerFunction :: c b a -> b -> Successor SomeException a

-- |The class of crawler which provide a function that supplies an initial
--  state.
class Crawler (c m) b a => StateCrawler c m b a where
   -- |Perform an IO action which supplies the crawler's initial state.
   crawlerState :: (MonadIO m, MonadError e m) => c m b a -> m a

-- |The class of crawlers which provide a function that supplies configuration.
class Crawler (c m) b a => ConfigurableCrawler c m b a where
   crawlerConfig :: (MonadIO m, MonadError e m) => c m b a -> m b

-- |The class of crawlers with a linear structure, i.e.
--  one whose 'Successor' functions generate at most one
--  successor node.
--
--  LinearCrawlers can go forwards and backwards. 'crawlerFunction'
--  will be assumed to go forwards, whereas its inverse, 'prevFunc',
--  is assumed to go backwards in the list of URLs which the
--  crawler traverses.
class Crawler c b a => LinearCrawler c b a where
   -- |Gets the URL of the first (earliest) item.
   firstURL :: c b a -> URL
   -- |Gets the URL of the last (latest) item.
   lastURL :: c b a -> URL
   -- |The the inverse of 'crawlerFunction' which goes backwards
   --  in the list of visited URLs. If b is 'CrawlerDirection', 'Backwards'
   --  inverts the directions of 'crawlerFunction' and 'prevFunc', causing
   -- 'prevFunc' to go forwards.
   prevFunc :: c b a -> b -> Successor SomeException a


-- Packing
--------------------------------------------------------------------------------

-- |"Packs" a crawler, getting rid of the type variable in both the
--  configuration (input) and the result (output). This is useful
--  when crawlers with heterogeneous states have to be stored together
--  in a collection.
packCrawler :: (MonadIO m, Functor m, MonadError e m, StateCrawler c m b a,
                ConfigurableCrawler c m b a, Typeable a, Functor f, Functor d)
            => c m b a -> (HList l -> c m b a -> b -> a -> f (d a))
            -> HList l -> m (f (d Dynamic))
packCrawler x f args = do conf <- crawlerConfig x
                          state <- crawlerState x
                          return $ fmap (fmap toDyn) $ f args x conf state

-- |"Packs" a crawler, getting rid of the configuration type variable.
packCrawler' :: (MonadIO m, Functor m, MonadError e m, StateCrawler c m b a,
                 ConfigurableCrawler c m b a)
             => c m b a -> (HList l -> c m b a -> b -> a -> m d) -> HList l -> m d
packCrawler' x f args = do conf <- crawlerConfig x
                           state <- crawlerState x
                           f args x conf state

-- Instances
--------------------------------------------------------------------------------

-- Crawler
--------------------------------------------------------------------------------
instance Crawler (TreeCrawler m) b a where
   crawlerName = tcName
   crawlerDomain = tcDomain
   crawlerFunction = tcSucc

instance Crawler (SimpleLinearCrawler m) CrawlerDirection (Maybe Int) where
   crawlerName SimpleLinearCrawler{slcName=n} = n
   crawlerDomain SimpleLinearCrawler{slcDomain=d} = d
   crawlerFunction SimpleLinearCrawler{slcContentXPath=content,
                                       slcNextXPath=next,
                                       slcPrevXPath=prev} =
      \case Forwards  -> simpleLinearSucc content next
            Backwards -> simpleLinearSucc content prev

simpleLinearSucc :: Text -> Text -> Successor SomeException (Maybe Int)
simpleLinearSucc xpContent xpLink = htmlSuccessor id
                                    $ simpleLinearSucc' xpContent xpLink

simpleLinearSucc' :: Text -> Text -> HTMLSuccessor SomeException (Maybe Int)
simpleLinearSucc' xpContent xpLink _ doc counter
   | isNothing counter || fromJust counter > 0 = (content, link)
   | otherwise = ([],[])
   where
      content = map (simpleNode counter' Blob)
                $ mapMaybe getText $ getXPathLeaves xpContent doc

      link = map (simpleNode counter' Inner)
             $ getSingleText
             $ getXPathLeaves xpLink doc
      counter' = fmap (\x -> x - 1) counter

-- StateCrawler
--------------------------------------------------------------------------------
instance StateCrawler TreeCrawler m b a where
   crawlerState = tcInit

instance (Functor m, MonadError SomeException m)
         => StateCrawler SimpleLinearCrawler
                         m CrawlerDirection
                         (Maybe Int) where
   crawlerState _ = ask' stateAsker >$> maybe Nothing Just
      where stateAsker = maybeAsker "Enter max. number of pages to get (or leave blank): "
                                    "Expected positive integer!"
                                    "Expected positive integer!"
                                    (return . (>= 0))

-- ConfigurableCrawler
--------------------------------------------------------------------------------
instance ConfigurableCrawler TreeCrawler m b a where
   crawlerConfig = tcConfig

instance (Functor m, MonadIO m, MonadError SomeException m)
         => ConfigurableCrawler SimpleLinearCrawler
                                m
                                CrawlerDirection
                                (Maybe Int) where
   crawlerConfig _ = ask' configAsker >$> maybe Forwards id
      where configAsker = maybeAsker "Enter direction (Forwards/Backwards; default=Forwards): "
                                     "Expected Forwards/Backwards."
                                     undefined
                                     (const $ return True)

-- LinearCrawler
--------------------------------------------------------------------------------

instance LinearCrawler (SimpleLinearCrawler m) CrawlerDirection (Maybe Int) where
   firstURL SimpleLinearCrawler{slcFirstURL=f} = f
   lastURL SimpleLinearCrawler{slcLastURL=l} = l
   prevFunc c = crawlerFunction c . invert
      where
         invert Forwards = Backwards
         invert Backwards = Forwards

-- Helper functions
--------------------------------------------------------------------------------


-- |Constructs a 'TreeCrawler' without state.
voidCrawler :: Monad m
              => Text
              -> WildcardURL
              -> Successor SomeException Void
              -> VoidCrawler m
voidCrawler name dom succ = TreeCrawler name
                                        dom
                                        (return undefined)
                                        (return undefined)
                                        (const succ)

-- |Constructs a 'TreeCrawler' with a state.
stateCrawler :: Monad m
              => Text
              -> WildcardURL
              -> Successor SomeException a
              -> m a
              -> TreeCrawler m Void a
stateCrawler name dom succ st = TreeCrawler name
                                            dom
                                            (return undefined)
                                            st
                                            (const succ)

-- |Constructs a 'TreeCrawler' with configuration.
configCrawler :: Monad m
              => Text
              -> WildcardURL
              -> (b -> Successor SomeException Void)
              -> m b
              -> TreeCrawler m b Void
configCrawler name dom succ co = TreeCrawler name
                                             dom
                                             co
                                             (return undefined)
                                             succ
