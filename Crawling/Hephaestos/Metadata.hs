{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Crawling.Hephaestos.Metadata where

import Prelude hiding (FilePath)

import Control.Arrow
import Control.Exception
import Control.Foldl (FoldM(..))
import Control.Lens (makeLenses, (&), (%~), (^.))
import Control.Monad (foldM, mzero, join)
import Control.Monad.Catch (throwM)
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
import Data.ListLike (ListLike(append), StringLike(fromString))
import Data.Maybe (isNothing, fromJust)
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
import Filesystem.Path.CurrentOS hiding (append, decode, encode, (<.>))

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types
import qualified Crawling.Hephaestos.Fetch.Successor as S
import Crawling.Hephaestos.Fetch.ErrorHandling

-- |A metadata node.
data MetaNode i =
   InnerNode{metaURL::URL}
   | Leaf{metaFile::T.Text,
          -- ^Filename under which the result is saved.
          --  Does not include the file extension and does not
          --  include any counters at the end of the filename
          --  (e.g. if there is a file @abc_3.error@, this field
          --   will have the value @abc@.)
          metaType::ResultType,
          -- ^Type of the result. This will be any value except 'Inner'.
          metaLeafURL::Maybe URL,
          -- ^URL from which the result stems. In general, this
          --  value will only exist if the result is a 'Blob',
          --  but various post-processing functions may set it.
          metaIdent :: Maybe i
          -- ^Identifier of the result.
          }

-- |The type of a FetchResult
data ResultType = Blob | Inner | PlainText | XmlResult | BinaryData | Failure | Info
   deriving (Show, Eq, Ord, Read, Enum)

-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON i => FromJSON (MetaNode i) where
   parseJSON (Object v) = do
      url <- v .:? "url"
      file <- v .:? "file"
      typ <- v .:? "type"
      ident <- v .:? "ident"
      if isNothing typ || typ == Just Inner
         then maybe mzero (return . InnerNode) url
         else return $ Leaf (fromJust file) (fromJust typ) url ident
   parseJSON _ = mzero

-- |parseJSON always returns 'Nothing'. This is useful if a field
--  can be of any type, but should be ignored.
instance FromJSON (Maybe Void) where
   parseJSON _ = return Nothing

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

-- |Dummy instance for Void which returns 'undefined'.
--  This should ONLY be used in cases where a 'Void' is part of a sum
--  type (e.g. @Maybe Void@) which only ever uses its other cases
--  (e.g. @Nothing@, never @Just@).
instance ToJSON Void where
   toJSON _ = error "called toJSON on type Void!"

instance ToJSON i => ToJSON (MetaNode i) where
   toJSON (InnerNode url) = object ["url" .= url]
   toJSON (Leaf file typ url ident) = object
                                      . addIdent
                                      . addURL
                                      $ ["file" .= file, "type" .= typ]
      where
         addURL xs = maybe xs (\y -> xs++["url" .= y]) url
         addIdent xs = maybe xs (\y -> xs++["ident" .= y]) ident

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
getType :: S.FetchResult e i -> ResultType
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

-- |Reads a metadata tree from file, throwing a 'DataFormatError' if
--  the contents can't be parsed.
readMetadata :: FromJSON i => FilePath -> IO (Tree (MetaNode i))
readMetadata metadataFile =
   BL.readFile (encodeString metadataFile)
   >$> decode
   >>= maybe (throwM parseErr) (return . fromJust)
   where
      parseErr = MetadataParsingError file'
      file' = either T.fromStrict T.fromStrict $ toText metadataFile

-- |A variant of 'M.readMetadata' that ignores any identifiers.
--  Consequently, the 'M.metaIdent' fields in the tree returned by this
--  function will always be 'Nothing', even if a value was present in
--  the metadata file.
readMetadata' :: FilePath -> IO (Tree (MetaNode Void))
readMetadata' = readMetadata >=$> fmap join'
   where join' :: MetaNode (Maybe Void) -> MetaNode Void
         join' (InnerNode url) = InnerNode url
         join' m@Leaf{metaIdent=i} = m{metaIdent = join i}

-- |Saves a tree to a metadata file.
saveMetadata :: ToJSON i
             => FilePath
                -- ^The filename of the metadata file.
             -> Path URI
                -- ^Path to the root of the tree. Use this if the given
                --  tree is a subtree some other one that has already been
                --  saved.
             -> Tree (S.SuccessorNodeSum coll SomeException i b)
                -- ^The tree (with UUIDs in the leaves, indicating the filename
                --  under which the leaf should be saved).
             -> IO ()
saveMetadata metadataFile path t = BL.writeFile (encodeString metadataFile)
                                   . encode
                                   . fmap to
                                   $ foldr mkNode t path
   where
      -- turns the given path into a tree going to t's root.
      mkNode n m = Node (S.InnerSuccessor undefined (S.Inner n id)) [m]

      -- turns a SuccessorNode into a MetaNode
      to :: S.SuccessorNodeSum coll e i a -> MetaNode i
      to (S.InnerSuccessor _ (S.Inner url _)) = InnerNode (fromString $ show url)
      to (S.LeafSuccessor _ ty@(S.Blob i url _) uuid _ _) =
         Leaf (fromString $ show uuid) (getType ty) (Just $ fromString $ show url) i
      to (S.LeafSuccessor _ ty uuid _ _) =
         Leaf (fromString $ show uuid) (getType ty) Nothing
         (if S.isFailure ty then Nothing else S.fetchIdent ty)

-- |Puts UUIDs to the leaves of an MTree which indicates the filename
--  to which each leaf should be saved.
--
--  Note that everything after the UUID-part of the LeafSuccessors will be
--  undefined.
putUUIDs :: MTree IO (S.SuccessorNode SomeException i b)
         -> IO (MTree IO (S.SuccessorNodeSum c SomeException i b))
putUUIDs = fmapM addUUID
   where
      addUUID (S.SuccessorNode st res)
         | S.isInner res = return (S.InnerSuccessor st res)
         | otherwise = do uuid <- nextRandom
                          return $ S.LeafSuccessor st res uuid undefined undefined

-- Creates a metadata file with an UUID filename in the given directory.
createMetaFile :: FilePath -> IO FilePath
createMetaFile saveLocation =
   do createDirectoryIfMissing True (encodeString saveLocation)
      x <- nextRandom
      return $ saveLocation </> decodeString ("metadata_" `append` show x `append` ".json")
