{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

-- |All-inclusive downloading. This module uses 'Crawling.Hephaestos.Fetch'
--  and 'Crawling.Hephaestos.Fetch.Tree' as building blocks to provide
--  all-in-one downloading, result extraction, saving, and retrying.
module Crawling.Hephaestos.Fetch.Forest where

import Prelude hiding ((++))

import Control.Arrow
import Control.Exception
import Control.Monad.Except
import Control.Foldl (FoldM(..))
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
import qualified Data.List.Safe as L
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Tree
import Data.Tree.Monadic
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Void
import Crawling.Hephaestos.Helper.String ((++))
import Data.Types.Isomorphic
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import System.Directory.Generic
import System.FilePath.Generic

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types
import qualified Crawling.Hephaestos.Fetch.Types.Metadata as M
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Helper.String (stripParams, showT)
import System.REPL

import Debug.Trace

--type DynNode = SuccessorNode SomeException Dynamic

-- |List- or setlike collections.
type Collection (c :: (* -> *)) (a :: *) =
   (Co.Foldable (c a) a,
    Co.Unfoldable (c a) a,
    Co.BulkInsertable [a] (c a))

-- |A wrapper around 'downloadForest' that runs a crawler based on a
--  successor function, an initial state, and a URL.
complexDownload :: (Collection results (Path URL, SuccessorNode SomeException b))
                => Manager
                -> (Request -> Request) -- ^The global request modifier.
                -> T.Text -- ^The root of the save path.
                -> Successor SomeException b -- ^Successor function.
                -> b -- ^Initial state for the successor function.
                -> URL -- ^Initial URL.
                -> ErrorIO (ForestResult results b)
complexDownload m reqF savePath succ initialState url =
   downloadForest m reqF savePath succ $ Co.singleton node
   where
      node = ([], SuccessorNode initialState Inner id url)

-- |Variant of 'complexDownload' that runs a crawler without a state.
complexDownload' :: (Collection results (Path URL, SuccessorNode SomeException Void))
                 => Manager
                 -> (Request -> Request) -- ^The global request modifier.
                 -> T.Text -- ^The root of the save path.
                 -> Successor SomeException Void -- ^Successor function.
                 -> URL -- ^Initial URL.
                 -> ErrorIO (ForestResult results Void)
complexDownload' m reqF savePath succ url =
   complexDownload m reqF savePath succ undefined url

-- |Results of a downloading operation.
data ForestResult coll b =
   ForestResult{
      -- |Leftover nodes of the download process, i.e. Failures and unknown
      --  node types that weren't handled.
      results :: coll (Path URL, SuccessorNode SomeException b),
      -- |The name of the created metadata file(s).
      metadataFiles :: [T.Text]
   }

-- |Takes a collection of 'Successor' nodes and tries to download & save
--  them to disk. A successfully downloaded node is removed from the input set.
--
--  == Re-tryable failure nodes
--  A 'Failure' is called re-tryable if one of the following conditions is met
--  (inductive def.):
--
--  * It has an original node that isn't a 'Failure';
--  * It has an original node that is a failure, but that node is re-tryable.
--  i.e. there must be a @(Just x)@ with @x@ being 'Blob', 'Inner', 'PlainText',
--  etc. at the end of the whole chain. That @x@ is called the __root__ of the
--  'Failure' nodes in the chain.
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
--  == Metadata
--  The metadata of the downloads will be saved as JSON in a text file.
--  This will be @metadata_[UUID].txt@, with @[UUID]@ being a fresh
--  UUID. __Each element of the input collection will result in a separate
--  metadata file.__
--
--  === Metadata JSON
--  The JSON format is as follows (pseudo-BCNF):
--
--  @
--  NODE       ::= INNER-NODE | LEAF
--  INNER-NODE ::= { "url": string, children: [NODE]}
--  LEAF       ::= { "url": string, "file": string, "type": TYPE}
--  TYPE       ::= "Blob" | "PlainText" | "XmlResult" | "BinaryData"
--                 | "Info" | "Failure"
--  NODE
--  @
--
--  If @metadata.txt@ already exists, the function will create
--  @metadata_[UUID].txt@ instead, where @[UUID]@ is a freshly generated
--  UUID. The output will contain the name of the created file.
--
--  The file contains a single node, with all others as descendents.
--  Each nodes contains its URL; the leaves contain the UUID and their file
--  type. The users of this function may leave the downloaded files as they
--  are, or they may perform transformations to clean up the data. For
--  common transformations, see 'Crawling.Hephaestos.Fetch.Transform'.
--
--  === Extensions
--  Saved files are given the following extensions:
--
--  * @.bin@ for 'Blob',
--  * @.txt@ for 'PlainText',
--  * @.xml@ for 'XmlResult',
--  * @.bin@ for 'BinaryData',
--  * @.info@ for 'Info'.
--
--  == Result set
--  The output set will only contain 'Failure' nodes (or be empty if there
--  were no failures) and those which aren't of the type mentioned above.
--  The failures can be used in another invocation of downloadForest.
--  If a node failed multiple times in a row, it will contain that history.
--  The first component of the result tuples is the path from the root of
--  the original fetch tree to the node's parent.
downloadForest :: forall a results b errors.
                  (Collection results (Path URL, SuccessorNode SomeException b))
               => Manager
               -> (Request -> Request) -- ^The global request modifier.
               -> T.Text -- ^The root of the save path.
               -> Successor SomeException b -- ^Successor function.
               -> results (Path URL, SuccessorNode SomeException b)
               -> ErrorIO (ForestResult results b)
downloadForest m reqMod saveLocation succ =
   Co.foldlM saveNode (ForestResult Co.empty [])
   where
      saveNode :: ForestResult results b
               -> (Path URL, SuccessorNode SomeException b)
               -> ErrorIO (ForestResult results b)
      -- |Save an entire fetch tree. First, we map UUIDs to the tree's
      --  leaves, then we materialize the whole thing.
      --
      --  After that, we do two things:
      --
      --  1. we store the tree's metadata (a JSON skeleton) in a JSON file, and
      --  2. we attempt to fetch and save all the leaves
      --
      --  The main drawback of this way of doing things is that the entire tree
      --  is kept in memory. For very large fetch trees (GB-sized), this can be
      --  a problem.
      saveNode fr (path, SuccessorNode st Inner reqF url) = do
         let mtree = fetchTree m succ (reqF.reqMod) st url
         metadataFile <- liftIO (createMetaFile saveLocation)
         -- |put UUIDs to the nodes, save metadata, materialize tree
         (failures,results) <- saveMetadata metadataFile path mtree
                               >$> leaves' (reverse path)
                               >$> L.partition isFailure'
                               >$> first (fmap two3)
         -- |save results, collect failures
         (ForestResult xs meta) <- foldM save fr results
         return $ ForestResult (failures `Co.bulkInsert` xs) (metadataFile:meta)
         where
            two3 (a,b,c) = (a,b)

            leaves' = leaves (\(n,uuid) r -> (reverse r,n,uuid))
                             -- the Nothing should always be the case, but I
                             -- left it there to let bugs cause pattern-match
                             -- failures.
                             (\(n,Nothing) r -> (nodeURL n):r)

            isFailure' (_,n,_) = isFailure $ nodeRes n
            save f (path', node, uuid) = saveLeaf' (Just $ showT uuid) f (path L.++ path') node

      -- Save leaf types
      saveNode fr (path,n) | isLeaf $ nodeRes n = saveLeaf' Nothing fr path n

      --Re-try failures if possible (and take tried filenames into account for leaves).
      saveNode fr (path, n@SuccessorNode{nodeRes=Failure _ (Just (orig, uuid))})
         | isFailure orig || isInner orig =
              saveNode fr (path, n{nodeRes=orig})
         | otherwise = saveLeaf' uuid fr path n{nodeRes=orig}

      --Leave all other nodes unchanged
      saveNode (ForestResult xs m) n = return $ ForestResult (Co.insert n xs) m

      -- Leaf saving functions, with a custom action for each leaf type.
      saveLeaf' :: Maybe T.Text
                -> ForestResult results b
                -> Path URL
                -> SuccessorNode SomeException b
                -> ErrorIO (ForestResult results b)
      saveLeaf' t fr path n@(SuccessorNode st Blob reqF url) =
         saveLeaf t fr path n (\uuid -> download m (reqF.reqMod) url
                                        >>= saveURL saveLocation url
                                                   (showT uuid++typeExt Blob))

      saveLeaf' t fr path n@SuccessorNode{nodeRes=r@(PlainText p), nodeURL=url} =
         saveLeaf t fr path n
                  (\uuid -> saveURL saveLocation url
                                    (showT uuid++typeExt r)
                                    (T.encodeUtf8 p))
      saveLeaf' t fr path n@SuccessorNode{nodeRes=r@(XmlResult p), nodeURL=url} =
         saveLeaf t fr path n
                  (\uuid -> saveURL saveLocation url
                                    (showT uuid++typeExt r)
                                    (B.encode p))
      saveLeaf' t fr path n@SuccessorNode{nodeRes=r@(BinaryData p), nodeURL=url} =
         saveLeaf t fr path n
                  (\uuid -> saveURL saveLocation url
                                    (showT uuid++typeExt r)
                                    p)
      saveLeaf' t fr path n@SuccessorNode{nodeRes=r@(Info k v), nodeURL=url} =
         saveLeaf t fr path n
                  (\uuid -> saveURL saveLocation url
                                    (showT uuid++typeExt r)
                                    (encode $ object ["key" .= k, "value" .= v]))

      -- |Saves a non-failure leaf.
      saveLeaf :: -- |The filename that should be used. If none is given, a
                  --  fresh UUID is generated, together with a new metadata file.
                  Maybe T.Text
               -> ForestResult results b -- ^The results so far
               -> Path URL
               -> SuccessorNode SomeException b -- ^The node to save.
               -- |The save action which saves the contents to file. The UUID is a
               --  hint for the file name. If none was given, a fresh one is
               --  generated.
               -> (T.Text -> ErrorIO ())
               -- |The new results. If the saving failed, they will contain a new
               --  failure node. If the metadata parameter was True, it will also
               --  contain the filename of the new metadata file.
               -> ErrorIO (ForestResult results b)
      -- create a new UUID
      saveLeaf uuid (ForestResult xs meta) path n action =
         (case uuid of Nothing -> createUUID
                       Just uuid' -> useUUID uuid')
         `catchError`
         (\x -> return $ ForestResult ((path, wrapFailure n x uuid) `Co.insert` xs) meta)

         where
            createUUID = do
               metadataFile <- liftIO $ createMetaFile saveLocation
               (Node (_,Just uuid) []) <- saveMetadata metadataFile path
                                                       (MTree $ return $ MNode n [])
               action $ showT uuid
               return $ ForestResult xs (metadataFile:meta)

            useUUID = action >=$> const (ForestResult xs meta)

saveMetadata :: T.Text -- ^The filename for the metadata file.
             -> Path URL -- ^Path to the beginning of the tree (may be empty)
             -> MTree ErrorIO' (SuccessorNode SomeException b)
             -> ErrorIO (Tree (SuccessorNode SomeException b, Maybe UUID))
saveMetadata metadataFile path t = do
   tree' <- fmapM (\n -> liftIO nextRandom >$> (n,) . Just) t >>= materialize
   let tree = foldr mkNode tree' path
   liftIO $ BL.writeFile (to metadataFile) $ encode $ fmap toMeta tree
   return tree
   where
      -- turns the given path into a tree going to t's root.
      mkNode n m = Node (SuccessorNode undefined undefined undefined n, Nothing) [m]

      -- converts the SuccessorNodes to MetaNodes, which can be saved as JSON
      toMeta (SuccessorNode _ _ _ url, Nothing) = M.InnerNode url
      toMeta (SuccessorNode _ ty _ url, Just uuid) =
         M.Leaf url (showT uuid ++ typeExt ty) $ M.getType ty

-- Wraps a node into a failure, given an exception.
wrapFailure :: SuccessorNode SomeException b
            -> SomeException
            -> Maybe T.Text -- ^Filename under which saving the file was attempted.
            -> SuccessorNode SomeException b
wrapFailure n@SuccessorNode{nodeRes=orig} e t = n{nodeRes=Failure e (Just (orig, t))}

-- Creates a metadata file with an UUID filename in the given directory.
createMetaFile :: T.Text -> IO T.Text
createMetaFile saveLocation =
   nextRandom >$> (\x -> to saveLocation </> "metadata_" ++ showT x ++ ".txt")
