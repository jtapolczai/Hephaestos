{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |All-inclusive downloading. This module uses "Crawling.Hephaestos.Fetch"
--  and "Crawling.Hephaestos.Fetch.Tree" as building blocks to provide
--  all-in-one downloading, result extraction, saving, and retrying.
--
--  The functionality here a bit complex. Thus, here its details:
--
--  The functions here just run a crawler, just as those in
--  "Crawling.Hephaestos.Fetch.Tree", but in addition to generating a fetch
--  tree, they also save the results. Given that we're dealing with remote
--  actions, a whole, whole lot can go wrong in the course of this process,
--  and this module is designed to deal with as many of those errors as possible.
--
--  == Basic saving (the optimal case)
--  If nothing goes wrong, then the following files will be created:
--
--  1. one for each leaf of a fetch tree,
--  2. one metadata file in JSON-format for every fetch tree, containing
--     a skeleton version of it, to enable post-fetch transformations, renamings,
--     etc.
--
-- __Note:__ Results will be given unique, fresh names in the form of
--           UUIDs (V4). The mapping between URL and filename is stored in the
--           metadata file.
--
--  == Error handling
--  We distinguish three kinds of errors:
--
--  1. Those generated by crawlers; these are saved as .error files.
--  2. Those generated because of network failures. These are re-tried once.
--     If that re-try fails, information about that failure is saved as a
--     .error files.
--  3. Local IO errors. These are deemed non-recoverable. No .error file is
--     created, since it is assumed that that would fail too. Instead, the
--     fetch process throws an exception.
--
--  === What if a result has to be re-tried multiple times?
--  Suppose we have an error of type 2, and that it is re-tried multiple
--  times. In that case, one numbered .error file will be created for each
--  attempt, i.e. @X.error, X_2.error,... ,X_n.error@. To avoid filling up
--  space with too many error files, the fetch options contain a \"maximum failures\"
--  setting. If @n+1@ would exceed that value, @X_n.error@ is overwritten instead.
--
--  === Technical details about failures
--  \"Errors generated by crawlers\" above refers specifically to __non-retryable__
--  errors.
--
--  [@re-tryable node@] A node is re-tryable iff
--
--      1. it is a non-failure node, or
--      2. it is a failure node and its original node is re-tryable.
--
--  \"Re-trying a node\" always means \"retrying its root\".
--
--  [@root of a node@] The root of a node @n@ is
--
--      1. the root of @n@'s original node, if @n@ is a failure node with an
--         original node, or
--      2. @n@ itself otherwise.
--
--  If @n@ is re-tryable, its root will be retried. If it isn't its root will
--  be the earliest failure.
--
-- == Metadata format
--
-- Each fetch tree generated a metadata file named @metadata_[UUID].json@.
-- This file contains a rose tree of metadata nodes.
--
-- [@Metadata node@] An inner node is an object with a @url@ field. A leaf
--                   is an object with the fields @file@ and @type@.
--
--                   The value of @type@ is one of
--
--                   * @Blob@
--                   * @PlainText@
--                   * @XmlResult@
--                   * @BinaryData@
--                   * @Info@
--                   * @Failure@
--
-- An example metadata file:
--
module Crawling.Hephaestos.Fetch.Forest (
   ForestResult(..),
   results,
   metadataFiles,
   downloadFolder,
   Collection,
   -- * Downloading
   complexDownload,
   complexDownload',
   downloadForest,
   ) where

import Prelude hiding (FilePath)

import Control.Concurrent.STM.Utils
import Control.Exception hiding (catch)
import Control.Lens ((&), (%~), (^.), (.~))
import Control.Monad.Catch (catch)
import Data.Aeson (ToJSON)
import qualified Data.Collections as Co
import Data.Functor.FunctorM
import Data.Functor.Monadic
import Data.ListLike (ListLike(append))
import Data.Monoid (mempty, mappend)
import Data.Tree
import Data.Tree.Monadic
import Data.UUID.V4 (nextRandom)
import Data.Void
import Network.HTTP.Conduit hiding (path, withManager)
import Network.URI (URI)
import Filesystem.Path.CurrentOS' hiding (append, encode)

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types
import qualified Crawling.Hephaestos.Metadata as M
import Crawling.Hephaestos.Fetch.Successor

-- |A wrapper around 'downloadForest' that runs a crawler based on a
--  successor function, an initial state, and a URL.
--  In addition, a new directory is created for the downloaded files.
--
--  This function does quite a lot. For details, see 'downloadForest'.
complexDownload :: (Collection results (Path URI, SuccessorNode SomeException i b),
                    ToJSON i)
                => FetchOptions
                -> Successor SomeException i b -- ^Successor function.
                -> b -- ^Initial state for the successor function.
                -> URI -- ^Initial URL.
                -> IO (ForestResult i results b)
complexDownload opts succ initialState url = do
   uuid <- nextRandom
   let opts' = opts & savePath %~ (</> (decodeString $ show uuid))
       node = ([], SuccessorNode initialState (Inner url mempty))
   downloadForest opts' succ $ Co.singleton node

-- |Variant of 'complexDownload' that runs a crawler without a state.
complexDownload' :: (Collection results (Path URI, SuccessorNode SomeException i Void),
                     ToJSON i)
                 => FetchOptions
                 -> Successor SomeException i Void -- ^Successor function.
                 -> URI -- ^Initial URL.
                 -> IO (ForestResult i results Void)
complexDownload' opts succ = complexDownload opts succ undefined

-- |Takes a collection of 'Successor' nodes and tries to download & save
--  them to disk. All successfully downloaded nodes are removed from the input set.
--
--  == Node handling
--  Nodes are handled in the following way:
--
--  * The Re-tryable failure nodes are re-tried with their root.
--  * Non-re-tryable failure nodes are kept as-is.
--  * Nodes that only require local IO action ('PlainText', 'BinaryData',
--    'XmlResult') are saved as local text/binary/xml files.
--  * 'Blob's are downloaded and saved.
--  * For 'Inner' nodes, we re-run 'fetchTree'.
--
--  Both local and remote IO errors wrap the corresponding node into
--  a 'Failure' and replace it in the node set.
--
--  == Saved files
--  Not every URL has a file that we can extract from it (examples being
--  @youtube.com/watch?v=AAAAAAAA@ and similar). Therefore, all saved
--  files are given a UUID filesname with an extension that indicates
--  their type.
--
--  == Result set
--  The output set will only contain 'Failure' nodes (or be empty if there
--  were no failures) and those which aren't of the type mentioned above.
--  The failures can be used in another invocation of downloadForest.
--  If a node failed multiple times in a row, it will contain that history.
--  The first component of the result tuples is the path from the root of
--  the original fetch tree to the node's parent.
downloadForest :: (Collection results (Path URI, SuccessorNode SomeException i b),
                   ToJSON i)
               => FetchOptions
               -> Successor SomeException i b -- ^Successor function.
               -> results (Path URI, SuccessorNode SomeException i b)
               -> IO (ForestResult i results b)
downloadForest opts succ nodes = do
   (results :: [ForestResult i results b]) <- parMapSTM saveNode nodes

   let frEmpty' = frEmpty & downloadFolder .~ (opts ^. savePath)
   return $ Co.foldr apFr frEmpty' results

   where
      --saveNode :: (Path URI, SuccessorNode SomeException i b)
      --         -> IO (ForestResult i results b)
      -- Inner nodes
      -------------------------------------------------------------------------
      -- Save an entire fetch tree. We first save the MTree (in parallel)
      -- and then we save the resultant regular Tree as a metadata file.
      saveNode (path, SuccessorNode st (Inner url reqMod)) = do
         let mtree = fetchTree (opts & reqFunc %~ (`mappend` reqMod)) succ st url

         -- put UUIDs and the path from the root into the leaves,
         -- then execute saveLeaf on each one.
         mtree' <- M.putUUIDs mtree
                   >$> traverseM mkPath (reverse path)
                   >>= fmapM putSaveAction

         -- this performs all the IO actions (except for saving the
         -- metadata file).
         -- NOTE: storing the result of materializePar keeps the whole tree
         -- in memory. This might be problematic for very large fetch trees.
         tree <- materializePar (opts ^. taskLimit) mtree'

         -- save the metadata
         metadataFile <- M.createMetaFile (opts ^. savePath)
         M.saveMetadata metadataFile path tree

         -- gather up the forestResults from the leaves and return
         let fr = frEmpty & metadataFiles .~ [metadataFile]
             frRet = foldr apFr' fr (justLeaves id tree)

         return frRet

         where
            apFr' (LeafSuccessor _ _ _ _ f') f = apFr f' f
            apFr' (InnerSuccessor _ _) f = f

            --putSaveAction :: SuccessorNodeSum results SomeException i b -> IO (SuccessorNodeSum results SomeException i b)
            putSaveAction n@InnerSuccessor{} = return n
            putSaveAction (LeafSuccessor st res uuid path' _) = do
               fr <- saveLeaf opts
                              (Just . decodeString . show $ uuid)
                              frEmpty
                              (path `append` path')
                              (SuccessorNode st res)
               return (LeafSuccessor st res uuid (path `append` path') fr)

      -- Leaves
      -------------------------------------------------------------------------
      saveNode (path, n@SuccessorNode{nodeRes=res}) | isLeaf res =
         saveLeaf opts name frEmpty path n{nodeRes=orig}
         where (orig, name) = nodeRoot res

-- Save helpers
-------------------------------------------------------------------------------

-- Wraps a node into a failure, given an exception.
wrapFailure :: Exception e => SuccessorNode SomeException i b
            -> e
            -> Maybe FilePath -- ^Filename under which saving the file was attempted.
            -> SuccessorNode SomeException i b
wrapFailure n@SuccessorNode{nodeRes=orig} e t = n{nodeRes=Failure (SomeException e) (Just (orig, t))}

-- |Gets the root of a node.
nodeRoot :: FetchResult e i -> (FetchResult e i, Maybe FilePath)
nodeRoot n@(Failure _ Nothing) = (n, Nothing)
nodeRoot (Failure _ (Just (orig, fn))) | isFailure orig = nodeRoot orig
                                       | otherwise = (orig, fn)
nodeRoot n = (n, Nothing)

-- |Saves a leaf to a file, creating a metadata file or a failre node if necessary.
--
--  This function is partial; inner nodes and nested failures are not allowed.
saveLeaf :: forall i results b.
            (Collection results (Path URI, SuccessorNode SomeException i b),
             ToJSON i)
         => FetchOptions
         -> Maybe FilePath -- ^The filename that should be used.
         -> ForestResult i results b -- ^The results so far
         -> Path URI
         -> SuccessorNode SomeException i b -- ^The node to save.
         -> IO (ForestResult i results b)
         -- ^The new results. If the saving failed, they will contain a new
         --  failure node. If the metadata parameter was True, it will also
         --  contain the filename of the new metadata file.
saveLeaf opts filename fr path n = do
   (filename', fr') <- maybe createName (return . (,fr)) filename
   (saveFile opts filename' (nodeRes n) >> return fr')
      `catch` handler fr' filename'
   where
      -- creates a new metadata file and a new filename for a node
      createName = do
         metadataFile <- M.createMetaFile (opts ^. savePath)
         uuid <- nextRandom
         let n' = LeafSuccessor (nodeState n) (nodeRes n) uuid undefined undefined
         M.saveMetadata metadataFile path (Node n' [])
         let fr' = fr & metadataFiles %~ (metadataFile:)
             fn  = decodeString $ show uuid
         return (fn, fr')

      -- The type is important! It specifices that ONLY HttpException should be caught!
      handler :: ForestResult i results b -> FilePath
              -> HttpException -> IO (ForestResult i results b)
      handler fr' fn x = do
         -- If a HTTP exception occurred, we save the error to file
         -- and insert a new failure node into the result set.
         let failureNode = wrapFailure n x (Just fn)
         saveFile opts fn $ nodeRes failureNode
         return $ fr' & results %~ Co.insert (path, failureNode)

-- Assorted helpers
-------------------------------------------------------------------------------

-- |Combines two ForestResults by combining the 'results' and 'metadataFiles'
--  fields. This function isn't quite associative because the 'downloadFolder'
--  of the second argument is kept and that of the first one is discarded.
--
--  If we ignore that, 'apFr' and 'frEmpty' form a monoid.
apFr :: (Collection c (Path URI, SuccessorNode SomeException i a))
        => ForestResult i c a -> ForestResult i c a -> ForestResult i c a
apFr f' f = f & results %~ Co.insertMany (f' ^. results)
              & metadataFiles %~ Co.insertMany (f' ^. metadataFiles)

-- |An empty ForestResult.
frEmpty :: (Collection c (Path URI, SuccessorNode SomeException i a))
        => ForestResult i c a
frEmpty = ForestResult Co.empty [] empty

mkPath :: [URI] -> SuccessorNodeSum results SomeException i b -> IO ([URI], SuccessorNodeSum results SomeException i b)
mkPath path (LeafSuccessor st r uuid _ c) =
   return (errP, LeafSuccessor st r uuid (reverse path) c)
mkPath path (InnerSuccessor st r) =
   return (innerURL r : path, InnerSuccessor st r)

errP = error "mkPath: accessed leaf state"
