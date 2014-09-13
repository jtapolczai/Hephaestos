{-# LANGUAGE DataKinds #-}

-- |General tree-crawlers which generate 'MTree's (monad trees)
--  of fetch results.
module Fetch.Tree where

import Prelude hiding (succ)

import Control.Arrow
import Control.Monad
import Control.Monad.Loops
import Data.List (partition)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void

import Fetch
import Fetch.Types.Successor
import XPath

-- |A monadic rose tree in which the child nodes are wrapped in a monad.
--  @l@ is the type of the keys in the leaf nodes and @n@ the type
--  of the keys in the inner nodes.
data MTree m l n = -- ^A leaf.
                   MLeaf{leafContent::l}
                   -- ^An internal nodes with a value and children
                   --  wrapped in a monad.
                   | MNode {nodeContent::n, nodeChildren::m [MTree m l n]}

type CrawlIterator = Maybe URL -> ErrorIO (Maybe (Maybe URL, Maybe URL))

fetchIterate :: Manager -> TextExtractor -> TextExtractor -> CrawlIterator
fetchIterate _ _ _ Nothing = return Nothing
fetchIterate m next item (Just url) = 
   do res <- download m url
      doc <- toDocument url res
      let nextLink = next doc
          itemLink = item doc
      return $! Just (itemLink, nextLink)

fetchList :: URL -> CrawlIterator -> ErrorIO [URL]
fetchList start iter =
   do list <- unfoldrM iter (Just start) 
      return $! catMaybes list


-- |General tree fetch which takes a successor (node-expander) function
--  and generates a monadic tree of crawled results.
--  Only Blob-type internal nodes will be expanded. All others will be
--  turned into leaves.
fetchTree :: Manager -- ^The connection manager.
          -> Successor a NetworkError -- ^Node-expanding function with state @a@.
          -> a -- ^Initial state to be given to the node-expanding function.
          -> URL -- ^The initial URL.
          -> MTree ErrorIO' (FetchResult a NetworkError) URL -- ^Resultant tree of crawl results.
fetchTree m succ state url = MNode url children
   where
      getInput (u,a) = (a, fromBlob u)

      children = do
         doc <- toDocument url =<< download m url

         let (leaves, nodes) = succ url doc state
             -- Turn non-blob nodes into leaves, since we can't expand them.
             (actualNodes, actualLeaves) = partition (isBlob.fst) nodes
             leaves' = map MLeaf leaves ++ map (MLeaf . fst) actualLeaves
             -- The recursive call occurs here
             nodes'  = map (uncurry (fetchTree m succ) . getInput) actualNodes
                    
         return $ leaves' ++ nodes'

-- |Stateless variant of 'fetchTree'. Convenient for when
--  the successor function does not need a state.
fetchTree' :: Manager
           -> Successor Void NetworkError
           -> URL
           -> MTree ErrorIO' (FetchResult Void NetworkError) URL
fetchTree' m succ = fetchTree m succ undefined

-- |Extracts all leaves from an 'MTree' which are 'Blob's.
--  See 'extractFromTree'
extractBlobs :: Monad m => MTree m (FetchResult a e) n -> m [URL]
extractBlobs = extractFromTree isBlob fromBlob

-- |Extracts all leaves from an 'MTree' which are 'PlainText's.
--  See 'extractFromTree'
extractPlainText :: Monad m => MTree m (FetchResult a e) n -> m [Text]
extractPlainText = extractFromTree isPlainText fromPlainText

-- |Extracts all leaves from an 'MTree' which are 'XmlResult's.
--  See 'extractFromTree'
extractXmlResults :: Monad m => MTree m (FetchResult a e) n -> m [XmlTree]
extractXmlResults = extractFromTree isXmlResult fromXmlResult

-- |Extracts all leaves from an 'MTree' which are 'Failure's.
--  See 'extractFromTree'
extractFailures :: Monad m => MTree m (FetchResult a e) n -> m [FetchResult a e]
extractFailures = extractFromTree isFailure id

-- |Extracts all leaves from an 'MTree' which are 'Info's. Returns key/value-pairs.
--  See 'extractFromTree'
extractInfo :: Monad m => MTree m (FetchResult a e) n -> m [(Text,Text)]
extractInfo = extractFromTree isInfo (infoKey &&& infoValue)

-- |Gets the leaf nodes from an 'MTree' from left to right,
--  going breadth-first.
--  Only nodes of type 'Leaf' are counted as leaf nodes,
--  nodes with an empty list of successors are not.
extractFromTree :: Monad m
                -- ^Test to determine whether the leaf should be extracted.
                => (FetchResult a e -> Bool)
                -- ^Function to apply to leaf which passes the test.
                -> (FetchResult a e -> b)
                -- ^The tree whose results to extract.
                -> MTree m (FetchResult a e) n
                -> m [b]
extractFromTree test from (MLeaf a) = return [from a | test a]
extractFromTree test from (MNode _ children) = children >>= foldM conc []
   where
      conc cur s = liftM (cur++) $ extractFromTree test from s