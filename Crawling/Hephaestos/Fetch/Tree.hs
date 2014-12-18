{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- |General tree-crawlers which generate 'MTree's (monad trees)
--  of fetch results. This module can be used in conjunction with
--  'Crawling.Hephaestos.Fetch' to run simple downloading jobs and report
--  errors.
--
--  A simple example would be the following function,
--  which runs a stateful crawler, prints the errors, and
--  tries to download a list of URLs to the user's home directory (on Unix):
--  @
--  do let successor = <specify the crawler's successor function>
--     manager <- <get manager>
--     url <- <ask the user for initial URL>
--     state <- <ask the user for an initial state, e.g. a counter>
--     results <- extractResults $ fetchTree manager successor id state
--     let (urls, errors) = (filter isBlob results, filter isFailure results)
--     mapM_ print errors
--     let downloader (SuccessorNode _ Blob reqMod url) = downloadSave m reqMod "~/" url
--     mapM_ downloader urls
--  @
--
-- This approach is often good enough, although it has its drawbacks:
-- * other result types, such as XML trees, are discarded;
-- * errors during the last @mapM_@ do not get caught;
-- * there is no mechanism for retrying failed downloads.
--
-- For more a "enterprisey" solution, see 'Crawling.Hephaestos.Fetch.Forest',
-- which pretty provides everything at the push of a single button.
module Crawling.Hephaestos.Fetch.Tree (
  -- *Main fetching functions
  MTree (..),
  fetchTree,
  fetchTree',
  -- * Result extraction
  -- |These functions take an 'MTree' and run it by extracting its contents
  --  into a list. 'extractFromTree' is the main function that does this. All
  --  others are convience functions which extract only one type of result,
  --  discarding all others.
  extractResults,
  extractFromTree,
  extractBlobs,
  extractPlainText,
  extractXmlResults,
  extractFailures,
  extractInfo,
  )where

import Prelude hiding (succ)

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Data.Either (partitionEithers)
import Data.Functor.Monadic
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text, unpack)
import qualified Data.Tree as Tr
import Data.Tree.Monadic
import Data.Void
import Network.HTTP.Conduit (Request)
import qualified Network.URI as N

import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.XPath

-- |General tree fetch which takes a successor (node-expander) function
--  and generates a monadic tree of crawled results.
--
--  Only internal nodes (Inner-constructor) returned by the given 'Successor'
--  function will be expanded. All others are considered leaves.
fetchTree :: forall a. Manager -- ^The connection manager.
          -> Successor SomeException a
          -- ^Node-expanding function with state @a@.
          -> (Request -> Request)
          -- ^Global modifiers to all requests. Parts of these may be
          --  overridden by the 'Successor' function.
          -> a -- ^Initial state to be given to the node-expanding function.
          -> URL -- ^The initial URL.
          -> MTree ErrorIO' (SuccessorNode SomeException a)
          -- ^Resultant tree of crawl results.
fetchTree m succ reqF = fetchTreeInner m succ reqF id
   where
      -- Has an added "reqLocal" parameter that modifies the HTTP request for
      -- one call
      fetchTreeInner m succ reqF reqLocal state url = MTree results
         where
            results :: ErrorIO (MNode ErrorIO' (SuccessorNode SomeException a))
            results = (do
               doc <- download m (reqLocal . reqF) url
                   -- neglecting Nothing here is OK because the call
                   -- to @download@ above already throws an error in case
                   -- of invalid URLs
               let (Just uri) = N.parseURI (unpack url)
                   -- run the successor function on the fetched document
                   (nodes, leaves) = partition (isInner.nodeRes) $ succ uri doc state
                   leaves' = map (MTree . leaf) leaves
                   -- The recursive call occurs here
                   nodes' = map recCall nodes

               return $ MNode this $ leaves' ++ nodes')
               `catchError`
               (\err -> leaf $ simpleNode state (Failure err $ Just (Inner,Nothing)) url)


            -- recursive call to fetchTreeInner
            recCall (SuccessorNode state _ reqMod nodeURL) =
               fetchTreeInner m succ reqF reqMod state nodeURL

            -- The current node.
            this = (SuccessorNode state Inner (reqLocal . reqF) url)

            -- creates a leaf MNode.
            leaf = return . flip MNode []

-- |Stateless variant of 'fetchTree'. Convenient for when
--  the successor function does not need a state.
fetchTree' :: Manager
           -> Successor SomeException Void
           -> (Request -> Request)
           -> URL
           -> MTree ErrorIO' (SuccessorNode SomeException Void)
fetchTree' m succ reqF  = fetchTree m succ reqF undefined

-- |A version of 'fetchTree' which only has the crawler, the configuration
--  data, and the initial state as "proper" arguments, and the rest ('Manager',
--  URL, and request modifier) as a 'HList'. This can be used in tandem with
--  'Crawling.Hephaestos.Crawlers.packCrawler' to hide the type variables
--  of tree crawlers behind a homogeneous interface, making it possible to
--  store them in a single collection.
{-packableFetchTree :: (ConfigurableCrawler c ErrorIO' b a,
                    StateCrawler c ErrorIO' b a)
                  => HList FetchTreeArgs
                  -> c ErrorIO' b a
                  -> b
                  -> a
                  -> (MTree ErrorIO' (SuccessorNode SomeException a))
packableFetchTree (HCons m (HCons req (HCons url HNil))) cr config state =
   fetchTree m (crawlerFunction cr config) req state url-}

-- |Extracts all leaves from an 'MTree'. See 'extractFromTree'.
extractResults :: (Functor m, Monad m)
               => MTree m (SuccessorNode e a) -> m [(SuccessorNode e a)]
extractResults = extractFromTree (not.isInner.nodeRes) id

-- |Extracts all nodes from an 'MTree' which are 'Blob's.
--  See 'extractFromTree'
extractBlobs :: (Functor m, Monad m)
             => MTree m (SuccessorNode e a) -> m [(URL, Request -> Request)]
extractBlobs = extractFromTree (isBlob.nodeRes) (nodeURL &&& nodeReqMod)

-- |Extracts all nodes from an 'MTree' which are 'PlainText's.
--  See 'extractFromTree'
extractPlainText :: (Functor m, Monad m)
                 => MTree m (SuccessorNode e a) -> m [(URL, Text)]
extractPlainText = extractFromTree (isPlainText.nodeRes) (nodeURL &&& fromPlainText.nodeRes)

-- |Extracts all nodes from an 'MTree' which are 'XmlResult's.
--  See 'extractFromTree'
extractXmlResults :: (Functor m, Monad m)
                  => MTree m (SuccessorNode e a) -> m [(URL, XmlTree)]
extractXmlResults = extractFromTree (isXmlResult.nodeRes) (nodeURL &&& fromXmlResult.nodeRes)

-- |Extracts all nodes from an 'MTree' which are 'Failure's.
--  See 'extractFromTree'
extractFailures :: (Functor m, Monad m)
                => MTree m (SuccessorNode e a) -> m [SuccessorNode e a]
extractFailures = extractFromTree (isFailure.nodeRes) id

-- |Extracts all nodes from an 'MTree' which are 'Info's. Returns key/value-pairs.
--  See 'extractFromTree'
extractInfo :: (Functor m, Monad m)
            => MTree m (SuccessorNode e a) -> m [(URL, (Text,Text))]
extractInfo = extractFromTree (isInfo.nodeRes) (nodeURL &&& (infoKey &&& infoValue) . nodeRes)

-- |Gets nodes from an 'MTree' from left to right, going breadth-first.
extractFromTree :: (Functor m, Monad m)
                => (a -> Bool) -- ^Test to determine whether the leaf should be extracted.
                -> (a -> b) -- ^Function to apply to leaf which passes the test.
                -> MTree m a -- ^The tree whose results to extract.
                -> m [b] -- ^Result list.
extractFromTree test from (MTree m) = m >>= \(MNode a children) -> rec a children
   where
      -- If the node is a leaf, filter based on the predicate and return.
      rec a [] = return [from a | test a]
      -- If there are children, recursively traverse them.
      rec a xs = mapM (extractFromTree test from) xs
                 >$> concat
                 >$> (if test a then (:) (from a) else id)
