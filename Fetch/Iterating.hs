module Fetch.Iterating where

import Prelude hiding (succ)

import Control.Monad
import Control.Monad.Loops
import Data.Maybe (catMaybes)
import Data.Text (unpack)
import Data.Void

import Fetch
import XPath

-- |A monadic rose tree in which the child nodes are wrapped in a monad.
--  @l@ is the type of the keys in the leaf nodes and @n@ the type
--  of the keys in the inner nodes.
data MTree m l n = MLeaf l | MNode n (m [MTree m l n])

type CrawlIterator = Maybe URL -> ErrorIO (Maybe (Maybe URL, Maybe URL))

fetchIterate :: Manager -> TextExtractor -> TextExtractor -> CrawlIterator
fetchIterate _ _ _ Nothing = return Nothing
fetchIterate m next item (Just url) = 
   do res <- download m url
      doc <- toDocument url res
      let nextLink = next doc
          itemLink = item doc
      return $! Just (liftM unpack itemLink, liftM unpack nextLink)

fetchList :: URL -> CrawlIterator -> ErrorIO [URL]
fetchList start iter =
   do list <- unfoldrM iter (Just start) 
      return $! catMaybes list


-- |General tree fetch which takes a successor (node-expander) function
--  and generates a monadic tree of crawled results.
fetchTree :: Manager -- ^The connection manager.
          -> Successor a -- ^Node-expanding function with state @a@.
          -> a -- ^Initial state to be given to the node-expanding function.
          -> URL -- ^The initial URL.
          -> MTree ErrorIO' URL URL -- ^Resultant tree of crawl results.
fetchTree m succ state url = MNode url children
   where
      children = do doc <- toDocument url =<< download m url

                    let (leaves, nodes) = succ doc state
                        leaves' = map MLeaf leaves
                        nodes'  = map (uncurry $ flip $ fetchTree m succ) nodes
                    
                    return $ leaves' ++ nodes'

-- |Stateless variant of 'fetchTree'. Convenient for when
--  the successor function does not need a state.
fetchTree' :: Manager
           -> Successor Void
           -> URL
           -> MTree ErrorIO' URL URL
fetchTree' m succ = fetchTree m succ undefined

-- |Gets the leaf nodes from an 'MTree' from left to right,
--  going breadth-first.
--  Only nodes of type 'Leaf' are counted as leaf nodes,
--  nodes with an empty list of successors are not.
flattenTree :: Monad m => MTree m URL n -> m [URL]
flattenTree (MLeaf a) = return [a]
flattenTree (MNode _ children) = children >>= foldM conc []
   where
      conc cur s = liftM (cur++) $ flattenTree s
