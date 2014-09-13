{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Crawlers for tree-like structures.
--  The added value of this module over 'Fetch.Tree' is
--  that it provides ready-made descriptors and configuration
--  for crawlers to easy execution.
module Galleries.Tree (
   TreeCrawler(..),
   DynTreeCrawler(dynCrawlerName, dynDomain, runCrawler),
   toDyn,
   voidCrawler,
   stateCrawler,
   configCrawler
   ) where

import Prelude hiding (succ)

import Data.Text hiding (map)
import Data.Void

import Fetch
import Fetch.Tree

-- |Descriptor for a general tree crawler.
--  'crawlerGetState' and 'crawlerGetConfig' are optional
--  and perform the IO actions (likely reading in lines)
--  necessary to get an initial state and a configuration from the
--  user.
--  This desriptor is designed to work with 'fetchTree', to which
--  it supplies the 'Successor' function.
data TreeCrawler a c =
   TreeCrawler{crawlerName::Text,
               domain::URL,
               crawlerGetInput::IO a,
               crawlerGetConfig::IO c,
               crawlerFunction::c -> Successor a
               }

-- |Packed version of 'TreeCrawler' without type variables.
--  It's 'runCrawler' runs 'crawlerGetConfig', then 'crawlerGetInput'),
--  and then calls 'fetchTree'. Useful for storing crawlers with
--  heterogeneous configuration and state types in collections.
data DynTreeCrawler =
   DynTreeCrawler{
                  -- ^The crawler's name.
                  dynCrawlerName::Text,
                  -- ^The crawler's domain.
                  dynDomain::URL,
                  -- ^Runs 'crawlerGetInput' and 'crawlerGetConfig'
                  -- and then calls 'fetchTree' with the results.
                  runCrawler::IO (Manager -> URL -> MTree ErrorIO' URL URL)}

-- |Packs a 'TreeCrawler' into a dynamic crawler.
toDyn :: TreeCrawler a c -> DynTreeCrawler
toDyn t = DynTreeCrawler{dynCrawlerName = crawlerName t,
                         dynDomain = domain t,
                         runCrawler = do conf <- crawlerGetConfig t
                                         st <- crawlerGetInput t
                                         let succ = crawlerFunction t conf
                                         return (\m u -> fetchTree m succ st u)}

-- |Constructs a 'TreeCrawler' without input or configuration.
voidCrawler :: Text -> URL -> Successor Void -> TreeCrawler Void Void
voidCrawler name dom succ = TreeCrawler name
                                        dom
                                        (return undefined)
                                        (return undefined)
                                        (const succ)

-- |Constructs a 'TreeCrawler' with a state, but no configuration
stateCrawler :: Text -> URL -> Successor a -> IO a -> TreeCrawler a Void
stateCrawler name dom succ st = TreeCrawler name
                                            dom
                                            st
                                            (return undefined)
                                            (const succ)

-- |Constructs a 'TreeCrawler' with configuration, but no state
configCrawler :: Text -> URL -> (c -> Successor Void) -> IO c -> TreeCrawler Void c
configCrawler name dom succ conf = TreeCrawler name
                                               dom
                                               (return undefined)
                                               conf
                                               succ
