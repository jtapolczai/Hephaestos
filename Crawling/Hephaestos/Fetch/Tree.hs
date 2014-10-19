{-# LANGUAGE DataKinds #-}

-- |General tree-crawlers which generate 'MTree's (monad trees)
--  of fetch results.
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
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Data.Either (partitionEithers)
import Data.Functor.Monadic
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void
import Network.HTTP.Conduit (Request)

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.XPath

-- |A monadic rose tree in which the child nodes are wrapped in a monad.
--  @l@ is the type of the keys in the leaf nodes and @n@ the type
--  of the keys in the inner nodes.
data MTree m n = -- |An internal nodes with a value and children
                   --  wrapped in a monad.
                   MNode {nodeContent::n, nodeChildren::m [MTree m n]}

instance (Functor m) => Functor (MTree m) where
   fmap f (MNode n ns) = MNode (f n) $ fmap (fmap (fmap f)) ns

-- |General tree fetch which takes a successor (node-expander) function
--  and generates a monadic tree of crawled results.
--  Only internal nodes (Inner-constructor) will be expanded. All others will be
--  turned into leaves.
fetchTree :: Manager -- ^The connection manager.
          -> Successor a [NetworkError] -- ^Node-expanding function with state @a@.
          -> (Request -> Request) -- ^Global modifiers to all requests.
                                  --  Parts of these may be overridden by
                                  --  the 'Successor' function.
          -> a -- ^Initial state to be given to the node-expanding function.
          -> URL -- ^The initial URL.
          -> MTree ErrorIO' (SuccessorNode a [NetworkError])
             -- ^Resultant tree of crawl results.
fetchTree m succ reqF = fetchTreeInner m succ reqF id

-- Has an added "reqLocal" parameter that modifies the HTTP request for
-- one call
fetchTreeInner m succ reqF reqLocal state url = MNode this children
   where
      recCall (SuccessorNode s r f) = fetchTreeInner m succ reqF f s (fromBlob r)

      -- The current node.
      this = (SuccessorNode state (Blob url) (reqLocal . reqF))

      leaf = flip MNode (return [])

      children = do
         -- Try to get URL...
         fetchResult <- (download m (reqLocal . reqF) url >$> Right)
                        `catchError` (return . Left)

         -- and create a failure node or a list of recursive calls,
         -- base on success
         case fetchResult of
            Left err -> return $ [leaf $ simpleNode state $ Failure url err]
            Right doc ->
               do let (leaves, nodes) = succ url doc state
                      -- Turn non-blob nodes into leaves, since we can't expand them.
                      (actualNodes, toLeaves) = partition (isInner.nodeRes) nodes
                      leaves' = map leaf $ leaves ++ toLeaves
                      -- The recursive call occurs here
                      mkInput (SuccessorNode s r m) = (m, s, fromInner r)

                      nodes' = map recCall actualNodes

                  return $ leaves' ++ nodes'

-- |Stateless variant of 'fetchTree'. Convenient for when
--  the successor function does not need a state.
fetchTree' :: Manager
           -> Successor Void [NetworkError]
           -> (Request -> Request)
           -> URL
           -> MTree ErrorIO' (SuccessorNode Void [NetworkError])
fetchTree' m succ reqF  = fetchTree m succ reqF undefined

-- |Extracts all leaves from an 'MTree'. See 'extractFromTree'.
extractResults :: Monad m => MTree m (SuccessorNode a e) -> m [(SuccessorNode a e)]
extractResults = extractFromTree (not.isInner.nodeRes) id

-- |Extracts all nodes from an 'MTree' which are 'Blob's.
--  See 'extractFromTree'
extractBlobs :: Monad m => MTree m (SuccessorNode a e) -> m [(URL, Request -> Request)]
extractBlobs = extractFromTree (isBlob.nodeRes) (fromBlob . nodeRes &&& nodeReqMod)

-- |Extracts all nodes from an 'MTree' which are 'PlainText's.
--  See 'extractFromTree'
extractPlainText :: Monad m => MTree m (SuccessorNode a e) -> m [Text]
extractPlainText = extractFromTree (isPlainText.nodeRes) (fromPlainText.nodeRes)

-- |Extracts all nodes from an 'MTree' which are 'XmlResult's.
--  See 'extractFromTree'
extractXmlResults :: Monad m => MTree m (SuccessorNode a e) -> m [XmlTree]
extractXmlResults = extractFromTree (isXmlResult.nodeRes) (fromXmlResult.nodeRes)

-- |Extracts all nodes from an 'MTree' which are 'Failure's.
--  See 'extractFromTree'
extractFailures :: Monad m => MTree m (SuccessorNode a e) -> m [SuccessorNode a e]
extractFailures = extractFromTree (isFailure.nodeRes) id

-- |Extracts all nodes from an 'MTree' which are 'Info's. Returns key/value-pairs.
--  See 'extractFromTree'
extractInfo :: Monad m => MTree m (SuccessorNode a e) -> m [(Text,Text)]
extractInfo = extractFromTree (isInfo.nodeRes) ((infoKey &&& infoValue) . nodeRes)

-- |Gets nodes from an 'MTree' from left to right, going breadth-first.
extractFromTree :: Monad m => (a -> Bool) -- ^Test to determine whether the leaf should be extracted.
                -> (a -> b) -- ^Function to apply to leaf which passes the test.
                -> MTree m a -- ^The tree whose results to extract.
                -> m [b] -- ^Result list.
extractFromTree test from (MNode a children) = children >>= rec
   where
      -- If the node is a leaf, filter based on the predicate and return.
      rec [] = return [from a | test a]
      -- If there are children, recursively traverse them.
      rec xs = liftM concat $ mapM (extractFromTree test from) xs
