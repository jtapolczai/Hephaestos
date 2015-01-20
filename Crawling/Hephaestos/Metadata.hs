{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Crawling.Hephaestos.Metadata where

import Prelude hiding (FilePath)

import Control.Arrow
import Control.Exception hiding (Handler, catches, catch)
import Control.Foldl (FoldM(..))
import Control.Lens (makeLenses, (&), (%~), (^.))
import Control.Monad (foldM, mzero)
import Control.Monad.Catch (catch)
import Control.Monad.Loops (dropWhileM)
import Data.Aeson
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
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.ListLike (ListLike(append), StringLike(fromString))
import Data.Maybe (fromJust)
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
import qualified Crawling.Hephaestos.Fetch.Successor as S
import Crawling.Hephaestos.Fetch.ErrorHandling

-- |A metadata node.
data MetaNode = InnerNode{metaURL::URL}
                | Leaf{metaFile::T.Text, metaType::ResultType, metaLeafURL::Maybe URL}

-- |The type of a FetchResult
data ResultType = Blob | Inner | PlainText | XmlResult | BinaryData | Failure | Info
   deriving (Show, Eq, Ord, Read, Enum)

-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON MetaNode where
   parseJSON (Object v) = do
      url <- v .:? "url"
      file <- v .:? "file"
      typ <- v .:? "type"
      if isNothing typ || typ == Just Inner
         then maybe mzero (return . InnerNode) url
         else return $ Leaf (fromJust file) (fromJust typ) url
   parseJSON _ = mzero

instance FromJSON ResultType where
   parseJSON (String s) = case T.toLower $ T.fromStrict s of
      "blob" -> return Blob
      "inner" -> return Inner
      "plaintext" -> return PlainText
      "xmlresult" -> return XmlResult
      "binarydata" -> return BinaryData
      "failure" -> return Failure
      "info" -> return Info
   parseJSON _ = mzero

instance ToJSON MetaNode where
   toJSON (InnerNode url) = object ["url" .= url]
   toJSON (Leaf file typ Nothing) = object ["file" .= file, "type" .= typ]
   toJSON (Leaf file typ url@(Just _)) = object ["file" .= file, "type" .= typ, "url" .= url]

instance ToJSON ResultType where
   toJSON Blob = String "Blob"
   toJSON Inner = String "Inner"
   toJSON PlainText = String "PlainText"
   toJSON XmlResult = String "XmlResult"
   toJSON BinaryData = String "BinaryData"
   toJSON Failure = String "Failure"
   toJSON Info = String "Info"

-- Helpers
-------------------------------------------------------------------------------

instance S.HasExt ResultType where
   ext Blob = "blob"
   ext Inner = "inner"
   ext PlainText = "txt"
   ext BinaryData = "bin"
   ext XmlResult = "xml"
   ext Failure = "error"
   ext Info = "info"

-- |Gets the type of a FetchResult
getType :: S.FetchResult e -> ResultType
getType S.Blob{} = Blob
getType S.Inner{} = Inner
getType S.PlainText{} = PlainText
getType S.XmlResult{} = XmlResult
getType S.BinaryData{} = BinaryData
getType S.Failure{} = Failure
getType S.Info{} = Info

isBlob :: ResultType -> Bool
isBlob Blob = True
isBlob _ = False

isInner :: ResultType -> Bool
isInner Inner = True
isInner _ = False

isPlainText :: ResultType -> Bool
isPlainText PlainText = True
isPlainText _ = False

isXmlResult :: ResultType -> Bool
isXmlResult XmlResult = True
isXmlResult _ = False

isBinaryData :: ResultType -> Bool
isBinaryData BinaryData = True
isBinaryData _ = False

isFailure :: ResultType -> Bool
isFailure Failure = True
isFailure _ = False

isInfo :: ResultType -> Bool
isInfo Info = True
isInfo _ = False

-- |Saves an MTree to a metadata file, creating UUIDs for the (non-'Inner')
--  leaves in the process.
saveMetadata :: FilePath -- ^The filename for the metadata file.
             -> Path URI -- ^Path to the beginning of the tree (may be empty)
             -> MTree IO (S.SuccessorNode SomeException b)
             -> IO (Tree (S.SuccessorNode SomeException b, Maybe UUID))
saveMetadata metadataFile path t = do
   tree <- fmapM addUUID t
           >>= materialize
           >$> flip (foldr mkNode) path -- append the tree to end of the given path
   --let tree = foldr mkNode tree' path
   BL.writeFile (encodeString metadataFile) $ encode $ fmap to tree
   return tree
   where
      --adds an UUID, but only to leaves
      addUUID n@S.SuccessorNode{S.nodeRes=S.Inner _ _} = return (n, Nothing)
      addUUID n = nextRandom >$> (n,) . Just

      -- turns the given path into a tree going to t's root.
      mkNode n m = Node (S.SuccessorNode undefined (S.Inner n undefined), Nothing) [m]

      to :: (S.SuccessorNode e a, Maybe UUID) -> MetaNode
      to (S.SuccessorNode _ (S.Inner url _), Nothing) = InnerNode (fromString $ show url)
      to (S.SuccessorNode _ ty@(S.Blob url _), Just uuid) =
         Leaf (fromString $ show uuid) (getType ty) (Just $ fromString $ show url)
      to (S.SuccessorNode _ ty, Just uuid) =
         Leaf (fromString $ show uuid) (getType ty) Nothing

-- Creates a metadata file with an UUID filename in the given directory.
createMetaFile :: FilePath -> IO FilePath
createMetaFile saveLocation =
   do createDirectoryIfMissing True (encodeString saveLocation)
      x <- nextRandom
      return $ saveLocation </> (decodeString $ "metadata_" `append` show x `append` ".txt")
