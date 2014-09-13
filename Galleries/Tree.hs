{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Crawlers for tree-like structures.
--  The added value of this module over 'Fetch.Tree' is
--  that it provides ready-made descriptors and configuration
--  for crawlers to easy execution.
module Galleries.Tree (
   TreeCrawler(..),
   VoidCrawler,
   voidCrawler,
   stateCrawler,
   configCrawler
   ) where

import Prelude hiding (succ)

import Data.Text hiding (map)
import Data.Void

import Fetch
import Fetch.Types.Successor

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
               crawlerFunction::c -> Successor a NetworkError
               }

-- |A TreeCrawler without a state or configuration.
type VoidCrawler = TreeCrawler Void Void

-- |Constructs a 'TreeCrawler' without input or configuration.
voidCrawler :: Text -> URL -> Successor Void NetworkError -> TreeCrawler Void Void
voidCrawler name dom succ = TreeCrawler name
                                        dom
                                        (return undefined)
                                        (return undefined)
                                        (const succ)

-- |Constructs a 'TreeCrawler' with a state, but no configuration
stateCrawler :: Text -> URL -> Successor a NetworkError -> IO a -> TreeCrawler a Void
stateCrawler name dom succ st = TreeCrawler name
                                            dom
                                            st
                                            (return undefined)
                                            (const succ)

-- |Constructs a 'TreeCrawler' with configuration, but no state
configCrawler :: Text -> URL -> (c -> Successor Void NetworkError) -> IO c -> TreeCrawler Void c
configCrawler name dom succ conf = TreeCrawler name
                                               dom
                                               (return undefined)
                                               conf
                                               succ
