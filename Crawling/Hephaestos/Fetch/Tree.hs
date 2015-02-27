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
-- which pretty much provides everything at the push of a single button.
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
  extractFromTree,
  extractFromTreePar,
  extractLeaves,
  extractLeavesPar,
  ) where

import Prelude hiding (succ)

import Control.Concurrent.STM.Utils
import Control.Lens ((^.), (%~), (&))
import Control.Monad.Catch
import Data.Functor.Monadic
import Data.List (partition)
import Data.ListLike.String (StringLike(fromString))
import Data.Tree.Monadic
import Data.Void
import Network.HTTP.Conduit (Request)
import Network.HTTP.Types.Header (hReferer)
import qualified Network.URI as N

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor

-- |General tree fetch which takes a successor (node-expander) function
--  and generates a monadic tree of crawled results.
--
--  Only internal nodes (Inner-constructor) returned by the given 'Successor'
--  function will be expanded. All others are considered leaves.
fetchTree :: forall i a. FetchOptions -- ^Configuration data.
          -> Successor SomeException i a
          -- ^Node-expanding function with state @a@.
          -> a -- ^Initial state to be given to the node-expanding function.
          -> N.URI -- ^The initial URL.
          -> MTree IO (SuccessorNode SomeException i a)
          -- ^Resultant tree of crawl results.
fetchTree opts succ s uri = fetchTree' succ (SuccessorNode s (Inner uri id))
   where
      fetchTree' :: Successor SomeException i a
                 -> SuccessorNode SomeException i a
                 -> MTree IO (SuccessorNode SomeException i a)
      fetchTree' succ node@(SuccessorNode state res@(Inner uri reqMod)) = MTree (
         (do let doc = downloadWhole (opts & reqFunc %~ (reqMod.)) uri
             successors <- succ uri doc state
                           >$> map (addRef' uri)
                           >$> map (cond (isInner.nodeRes)
                                         (fetchTree' succ)
                                         (MTree . mkLeaf))

             return $ MNode node successors)
            `catch`
               (\err -> mkLeaf $ SuccessorNode state $ Failure err $ Just (res,Nothing)))
      fetchTree' _ _ = error "invalid pattern for fetchTree"

      mkLeaf = return . flip MNode []

      -- |@if f x then ifTtrue x else ifFalse x@, as a function.
      cond :: forall a b.(a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
      cond f ifTrue ifFalse x = if f x then ifTrue x else ifFalse x

      addRef :: N.URI -> Request -> Request
      addRef uri | opts ^. addReferer = addHeader hReferer (fromString $ show uri)
                 | otherwise          = id

      addRef' :: N.URI -> SuccessorNode SomeException i a -> SuccessorNode SomeException i a
      addRef' uri (SuccessorNode s (Inner u r)) = SuccessorNode s $ Inner u (r.addRef uri)
      addRef' uri (SuccessorNode s (Blob i u r)) = SuccessorNode s $ Blob i u (r.addRef uri)
      addRef' _ x = x

-- |Stateless variant of 'fetchTree'. Convenient for when
--  the successor function does not need a state.
fetchTree' :: FetchOptions
           -> Successor SomeException i Void
           -> N.URI
           -> MTree IO (SuccessorNode SomeException i Void)
fetchTree' opts succ  = fetchTree opts succ undefined

-- |Extracts all leaves from an 'MTree'. See 'extractFromTree'.
extractLeaves :: (Functor m, Monad m)
              => MTree m (SuccessorNode e i a) -> m [SuccessorNode e i a]
extractLeaves = extractFromTree (not.isInner.nodeRes) id

-- |Extracts all leaves from an 'MTree'. See 'extractFromTreePar'.
extractLeavesPar :: TaskLimit
                 -> MTree IO (SuccessorNode e i a)
                 -> IO [SuccessorNode e i a]
extractLeavesPar n = extractFromTreePar n (not.isInner.nodeRes) id

-- |Gets nodes from an 'MTree' from left to right, going depth-first.
extractFromTree :: (Functor m, Monad m)
                => (a -> Bool) -- ^Test to determine whether the leaf should be extracted.
                -> (a -> b) -- ^Function to apply to leaf which passes the test.
                -> MTree m a -- ^The tree whose results to extract.
                -> m [b] -- ^Result list.
extractFromTree test from tree = materialize tree
                                 >$> leaves addIf addIf []
                                 >$> concat
   where
      addIf s n = [from n | test n]++s

-- |Gets nodes from an 'MTree', going depth-first and processing nodes in
--  parallel. See 'Data.Tree.Monadic.materializePar'.
extractFromTreePar :: TaskLimit
                      -- ^The upper limit on concurrent tasks.
                   -> (a -> Bool)
                   -> (a -> b)
                   -> MTree IO a
                   -> IO [b]
extractFromTreePar n test from tree = materializePar n tree
                                      >$> leaves addIf addIf []
                                      >$> concat
   where
      addIf s n = [from n | test n]++s
