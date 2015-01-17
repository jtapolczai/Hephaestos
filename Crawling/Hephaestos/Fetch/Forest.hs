{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

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
   Collection(..),
   -- * Downloading
   complexDownload,
   complexDownload',
   downloadForest,
   ) where

import Prelude hiding (FilePath)

import Control.Arrow
import Control.Exception hiding (Handler, catches, catch)
import Control.Foldl (FoldM(..))
import Control.Lens (makeLenses, (&), (%~), (^.))
import Control.Monad.Except
import Control.Monad.Loops (dropWhileM)
import Data.Aeson (object, (.=), encode)
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Collections as Co
import qualified Data.Collections.BulkInsertable as Co
import qualified Data.Collections.Instances as Co
import Data.Char (toLower)
import Data.Dynamic
import Data.Functor.FunctorM
import Data.Functor.Monadic
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import Data.ListLike (ListLike(append), StringLike(fromString))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Tree
import Data.Tree.Monadic
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Void
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import Network.URI (URI)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Directory.Generic
import Filesystem.Path.CurrentOS hiding (append, encode, (<.>))

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types
import qualified Crawling.Hephaestos.Fetch.Types.Metadata as M
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Fetch.ErrorHandling
import System.REPL

import Debug.Trace

-- |Results of a downloading operation.
data ForestResult coll b =
   ForestResult{
      -- |Leftover nodes of the download process, i.e. Failures and unknown
      --  node types that weren't handled.
      _results :: coll (Path URI, SuccessorNode SomeException b),
      -- |The name of the created metadata file(s).
      _metadataFiles :: [FilePath],
      -- |Folder into which the files were downloaded.
      _downloadFolder :: FilePath
   }

makeLenses ''ForestResult

-- |List- or setlike collections.
type Collection (c :: (* -> *)) (a :: *) =
   (Co.Foldable (c a) a,
    Co.Unfoldable (c a) a,
    Co.BulkInsertable [a] (c a))

-- |A wrapper around 'downloadForest' that runs a crawler based on a
--  successor function, an initial state, and a URL.
--  In addition, a new directory is created for the downloaded files.
--
--  This function does quite a lot. For details, see 'downloadForest'.
complexDownload :: (Collection results (Path URI, SuccessorNode SomeException b))
                => FetchOptions
                -> Successor SomeException b -- ^Successor function.
                -> b -- ^Initial state for the successor function.
                -> URI -- ^Initial URL.
                -> ErrorIO (ForestResult results b)
complexDownload opts succ initialState url = do
   uuid <- liftIO nextRandom
   let opts' = opts & savePath %~ (</> (decodeString $ show uuid))
       node = ([], SuccessorNode initialState (Inner url) id)
   downloadForest opts' succ $ Co.singleton node

-- |Variant of 'complexDownload' that runs a crawler without a state.
complexDownload' :: (Collection results (Path URI, SuccessorNode SomeException Void))
                 => FetchOptions
                 -> Successor SomeException Void -- ^Successor function.
                 -> URI -- ^Initial URL.
                 -> ErrorIO (ForestResult results Void)
complexDownload' opts succ url =
   complexDownload opts succ undefined url

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
downloadForest :: forall a results b errors.
                  (Collection results (Path URI, SuccessorNode SomeException b))
               => FetchOptions
               -> Successor SomeException b -- ^Successor function.
               -> results (Path URI, SuccessorNode SomeException b)
               -> ErrorIO (ForestResult results b)
downloadForest opts succ =
   Co.foldlM saveNode (ForestResult Co.empty [] $ opts ^. savePath)
   where

      saveNode :: (Collection results (Path URI, SuccessorNode SomeException b))
               => ForestResult results b -> (Path URI, SuccessorNode SomeException b)
               -> ErrorIO (ForestResult results b)
      -- Inner nodes
      -------------------------------------------------------------------------
      -- Save an entire fetch tree. First, we map UUIDs to the tree's
      -- leaves, then we materialize the whole thing.
      -- After that, we do two things:
      --
      --  1. save the metadata and
      --  2. attempt to fetch and save all the leaves
      --
      --  The main drawback of this way of doing things is that the entire tree
      --  is kept in memory. For very large fetch trees (GB-sized), this can be
      --  a problem.
      saveNode fr (path, SuccessorNode st (Inner url) reqMod) = do
         let mtree = fetchTree (opts & reqFunc %~ (reqMod.)) succ st url
         metadataFile <- createMetaFile (opts ^. savePath)
         -- put UUIDs to the nodes, save metadata, materialize tree
         (failures,_,goodRes) <- saveMetadata metadataFile path mtree
                                 >$> leaves' (reverse path)
                                 -- we re-try the failures, throw away the
                                 -- dead ends (inners which are leaves) and
                                 -- save the results
                                 >$> partition3 isFailure' isInner'
                                 >$> first3 (fmap two3)
         fr' <- foldM save fr goodRes
         return $ fr' & results %~ (Co.bulkInsert failures)
                      & metadataFiles %~ (metadataFile:)
         where
            --go through the tree and add the path from the root to each result
            --node. This is done so that the leaves "remember" the relevant section
            --of the original fetch tree. That way, when we re-try a leaf after
            --a failure, we will know where in the original fetch tree it would
            --have fit.
            leaves' = leaves leafFn (\(n,Nothing) r -> (innerURL $ nodeRes n):r)

            -- we have to take two cases into account: 1) the leaf is a result-
            -- node and 2) the leaf is an inner node
            leafFn (n, Just uuid) r = (reverse r, n, uuid)
            leafFn (n, Nothing) r = (reverse r, n, undefined)

            save f (path', node, uuid) =
               saveLeaf opts (Just $ decodeString $ show uuid) f
                             (path `append` path') node

      -- Leaves
      -------------------------------------------------------------------------
      saveNode fr (path, n@SuccessorNode{nodeRes=res}) | isLeaf res =
         case nodeRoot res of
            (orig,Nothing)   -> saveLeaf opts Nothing fr path n{nodeRes=orig}
            (orig,Just name) -> saveLeaf opts (Just name) fr path n{nodeRes=orig}


-- Save helpers
-------------------------------------------------------------------------------

-- |Saves an MTree to a metadata file, creating UUIDs for the (non-'Inner')
--  leaves in the process.
saveMetadata :: FilePath -- ^The filename for the metadata file.
             -> Path URI -- ^Path to the beginning of the tree (may be empty)
             -> MTree ErrorIO' (SuccessorNode SomeException b)
             -> ErrorIO (Tree (SuccessorNode SomeException b, Maybe UUID))
saveMetadata metadataFile path t = do
   tree <- fmapM addUUID t
           >>= materialize
           >$> flip (foldr mkNode) path -- append the tree to end of the given path
   --let tree = foldr mkNode tree' path
   catchIO $ BL.writeFile (encodeString metadataFile) $ encode $ fmap toMeta tree
   return tree
   where
      --adds an UUID, but only to leaves
      addUUID n@SuccessorNode{nodeRes=Inner _} = return (n, Nothing)
      addUUID n = liftIO nextRandom >$> (n,) . Just

      -- turns the given path into a tree going to t's root.
      mkNode n m = Node (SuccessorNode undefined (Inner n) undefined, Nothing) [m]

      -- converts the SuccessorNodes to MetaNodes, which can be saved as JSON
      toMeta (SuccessorNode _ (Inner url) _, Nothing) = M.InnerNode (fromString $ show url)
      toMeta (SuccessorNode _ ty@(Blob url) _, Just uuid) =
         M.Leaf (fromString $ show uuid) (M.getType ty) (Just $ fromString $ show url)
      toMeta (SuccessorNode _ ty _, Just uuid) =
         M.Leaf (fromString $ show uuid) (M.getType ty) Nothing

-- Wraps a node into a failure, given an exception.
wrapFailure :: Exception e => SuccessorNode SomeException b
            -> e
            -> Maybe FilePath -- ^Filename under which saving the file was attempted.
            -> SuccessorNode SomeException b
wrapFailure n@SuccessorNode{nodeRes=orig} e t = n{nodeRes=Failure (SomeException e) (Just (orig, t))}

-- Creates a metadata file with an UUID filename in the given directory.
createMetaFile :: FilePath -> ErrorIO FilePath
createMetaFile saveLocation =
   do catchIO (createDirectoryIfMissing True $ encodeString saveLocation)
      x <- liftIO nextRandom
      return $ saveLocation </> (decodeString $ "metadata_" `append` show x `append` ".txt")

-- |Gets the root of a node.
nodeRoot :: FetchResult e -> (FetchResult e, Maybe FilePath)
nodeRoot n@(Failure _ Nothing) = (n, Nothing)
nodeRoot n@(Failure _ (Just (orig, fn))) | isFailure orig = nodeRoot orig
                                         | otherwise = (orig, fn)
nodeRoot n = (n, Nothing)

-- |Saves a leaf to a file, creating a metadata file or a failre node if necessary.
--
--  This function is partial; inner nodes and nested failures are not allowed.
saveLeaf :: forall results b.(Collection results (Path URI, SuccessorNode SomeException b))
         => FetchOptions
         -> Maybe FilePath -- ^The filename that should be used.
         -> ForestResult results b -- ^The results so far
         -> Path URI
         -> SuccessorNode SomeException b -- ^The node to save.
         -> ErrorIO (ForestResult results b)
         -- ^The new results. If the saving failed, they will contain a new
         --  failure node. If the metadata parameter was True, it will also
         --  contain the filename of the new metadata file.
saveLeaf opts filename fr path n = do
   (filename', fr') <- maybe createName (return . (,fr)) filename
   (saveAction opts n filename' >> return fr') `catch` (handler fr' filename')
   where
      -- creates a new metadata file and a new filename for a node
      createName = do
         metadataFile <- createMetaFile (opts ^. savePath)
         (Node (_,Just uuid) []) <- saveMetadata metadataFile path
                                                 (MTree $ return $ MNode n [])
         let fr' = fr & metadataFiles %~ (metadataFile:)
             fn  = decodeString $ show uuid
         return $ (fn, fr')

      -- The type is important! It specifices that ONLY HttpException should be caught!
      handler :: ForestResult results b -> FilePath
              -> HttpException -> ErrorIO (ForestResult results b)
      handler fr' fn x = do
         -- If a HTTP exception occurred, we save the error to file
         -- and insert a new failure node into the result set.
         let failureNode = wrapFailure n x (Just fn)
         saveAction opts failureNode fn
         return $ fr' & results %~ Co.insert (path, failureNode)

-- |Saves a leaf to a file.
--
--  This function is partial; inner nodes are not allowed.
--  In the case of failures, the __outermost__ failure
--  (not the root) will be saved.
saveAction :: FetchOptions -> SuccessorNode SomeException b -> FilePath -> ErrorIO ()
saveAction opts (SuccessorNode _ r@(Blob url) reqMod) uuid =
   download (opts ^. manager) (reqMod.(opts ^. reqFunc)) url
   >>= saveURL (opts ^. savePath) (uuid <.> ext r)

saveAction opts (SuccessorNode _ r@(PlainText p) _) name =
   saveURL (opts ^. savePath) (name <.> ext r) (T.encodeUtf8 p)
saveAction opts (SuccessorNode _ r@(XmlResult p) _) name =
   saveURL (opts ^. savePath) (name <.> ext r) (B.encode p)
saveAction opts (SuccessorNode _ r@(BinaryData p) _) name =
   saveURL (opts ^. savePath) (name <.> ext r) p
saveAction opts (SuccessorNode _ r@(Info k v) _) name =
   saveURL (opts ^. savePath) (name <.> ext r)
           (encode $ object ["key" .= k, "value" .= v])
saveAction opts (SuccessorNode _ r@(Failure e _) _) name =
   do (name':_) <- dropWhileM (catchIO . doesFileExist) (map mkName [1..])
      saveURL (opts ^. savePath) (decodeString name' <.> ext r) (T.encodeUtf8 $ T.pack $ show e)
   where
      mkName :: Integer -> String
      mkName x = encodeString
         $ (opts ^. savePath)
         </> (decodeString $ encodeString name `append` "_" `append` show x)
         <.> ext r

-- Assorted helpers
-------------------------------------------------------------------------------

two3 :: (a,b,c) -> (a,b)
two3 (a,b,c) = (a,b)

partition3 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a],[a],[a])
partition3 _ _ [] = ([],[],[])
partition3 f g (x:xs) | f x       = (x:one, two, three)
                      | g x       = (one, x:two, three)
                      | otherwise = (one, two, x:three)
   where (one,two,three) = partition3 f g xs

isFailure' (_,n,_) = isFailure $ nodeRes n
isInner' (_,n,_) = isInner $ nodeRes n

first3 :: (a -> d) -> (a,b,c) -> (d,b,c)
first3 f (a,b,c) = (f a, b, c)

-- |Synonym for addExtension
(<.>) :: FilePath -> T.Text -> FilePath
(<.>) x y = x `addExtension` (T.toStrict y)
