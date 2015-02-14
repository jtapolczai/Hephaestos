{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- |Contains the 'Successor' type which is used to
--  construct crawl trees, together with a set of useful
--  helper functions for creating 'Successor' functions.
--
--  For examples of successor functions, see @Galleries.Examples@.
module Crawling.Hephaestos.Fetch.Successor (
   -- *Types
   ForestResult(..),
   results,
   metadataFiles,
   downloadFolder,
   Successor,
   HTMLSuccessor,
   FetchResult(..),
   blob,
   plainText,
   binaryData,
   xmlResult,
   info,
   failure,
   isLeaf,
   SuccessorNode(..),
   SuccessorNode',
   SuccessorNodeSum(..),
   HasExt(..),
   -- *Helper functions relating to state and failure
   voidNode,
   noneAsFailure,
   -- *Discriminator functions
   getURL,
   getError,
   isBlob,
   isInner,
   isBinaryData,
   isPlainText,
   isXmlResult,
   isFailure,
   isInfo,
   -- * Adding HTTP headers
   addHeader,
   ) where

import Prelude hiding (lex, FilePath)

import Control.Exception
import Control.Lens (makeLenses)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Functor.Monadic
import qualified Data.List.Safe as LS
import Data.Maybe (fromMaybe)
import Data.Text.Lazy hiding (pack, toStrict)
import Data.Void
import Data.UUID (UUID)
import Filesystem.Path.CurrentOS
import Network.HTTP.Conduit (Request, requestHeaders)
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)
import Text.XML.HXT.DOM.TypeDefs

import Crawling.Hephaestos.Fetch.Types

-- |A function which extracts a number of successor nodes from a page.
type Successor e i a = URI -- ^The URI of the input
                       -> IO ByteString -- ^The input as a byte string.
                       -> a -- ^The input state.
                       -> IO [SuccessorNode e i a]
                          -- ^The list of resultant leaves and nodes.

-- |A Successor which only works on HTML input. See 'htmlSuccessor'.
type HTMLSuccessor e i a = URI
                           -> XmlTree
                           -> a
                           -> IO [SuccessorNode e i a]

-- |Result of a fetching operation.
data FetchResult e i =
   -- |A URL which is to be downloaded.
   Blob{fetchIdent::Maybe i, blobURL::URI, reqMod::Request -> Request}
   -- |An inner node in a search treee.
   | Inner{innerURL::URI, reqMod::Request -> Request}
   -- |Some plain text without any deeper semantic value.
   | PlainText{fetchIdent::Maybe i, fromPlainText::Text}
   -- |A ByteString (binary data).
   | BinaryData{fetchIdent::Maybe i, fromBinary::ByteString}
   -- |An XML tree.
   | XmlResult{fetchIdent::Maybe i, fromXmlResult::XmlTree}
   -- |A failure which stores an error and the original node, if present.
   | Failure{failureError::e,
             -- ^The error which occurred.
             originalNode::Maybe (FetchResult e i, Maybe FilePath)
             -- ^If applicable, the original node which couldn't be saved.
             --  This is most useful in the case of 'Blob's.
            }
   -- |A piece of named auxiliary information, such as a title or an author.
   | Info{fetchIdent::Maybe i, infoKey::Text,infoValue::Text}

-- |A node generated by a 'Successor' function.
--  See 'simpleNode', 'voidNode' and 'redNode' for smart constructors.
data SuccessorNode e i a = SuccessorNode {nodeState::a,
                                          -- ^The new state.
                                          nodeRes::FetchResult e i
                                          -- ^The node's result.
                                         }

-- |Results of a downloading operation.
data ForestResult i coll b =
   ForestResult{
      -- |Leftover nodes of the download process, i.e. Failures and unknown
      --  node types that weren't handled.
      _results :: coll (Path URI, SuccessorNode SomeException i b),
      -- |The name of the created metadata file(s).
      _metadataFiles :: [FilePath],
      -- |Folder into which the files were downloaded.
      _downloadFolder :: FilePath
   }

makeLenses ''ForestResult

-- |A version of 'SuccessorNode' that distinguishes between inner nodes and
--  leaf nodes. Leaf nodes have a UUID and a path from the root (needed by
--  "Crawling.Hephaestos.Fetch.Forest").
data SuccessorNodeSum coll e i a =
   InnerSuccessor a (FetchResult e i)
   | LeafSuccessor a (FetchResult e i) UUID (Path URI) (ForestResult i coll a)

-- |Shorthand for @SuccessorNode SomeException i a@
type SuccessorNode' i a = SuccessorNode SomeException i a

-- |Functor instance over the node state.
instance Functor (SuccessorNode e i) where
   fmap f s@SuccessorNode{nodeState=a} = s{nodeState=f a}

-- |Selects the first non-EQ element from a list or EQ if no such element exists.
lex :: [Ordering] -> Ordering
lex = fromMaybe EQ . LS.head . LS.dropWhile (EQ==)

-- |Adds a request header. If a header with the same name is already present,
--  it is replaced.
addHeader :: HeaderName -> ByteString -> Request -> Request
addHeader k v r = r{requestHeaders=headers}
   where headers = replace ((k==) . fst) (k,toStrict v) $ requestHeaders r
         -- |Replaces the first element in a list which satisfies a predicate.
         replace :: (a -> Bool) -> a -> [a] -> [a]
         replace _ _ [] = []
         replace test y (x:xs) | test x    = y:xs
                               | otherwise = x : replace test y xs

-- |Creates a 'SuccessorNode' from a 'FetchResult' and no state.
--  No request modifiers will be applied.
voidNode :: FetchResult e i -> SuccessorNode e i Void
voidNode = SuccessorNode undefined

-- |Transforms the identifier of a 'FetchResult'.
instance Functor (FetchResult e) where
   fmap f (Blob i url r) = Blob (fmap f i) url r
   fmap _ (Inner u r) = Inner u r
   fmap f (PlainText i t) = PlainText (fmap f i) t
   fmap f (BinaryData i t) = BinaryData (fmap f i) t
   fmap f (XmlResult i t) = XmlResult (fmap f i) t
   fmap _ (Failure e Nothing) = Failure e Nothing
   fmap f (Failure e (Just (r, fp))) = Failure e $ Just (fmap f r, fp)
   fmap f (Info i k v) = Info (fmap f i) k v

-- |Gets the URL of a Blob or an Inner. For all other result types, returns Nothing.
getURL :: FetchResult e i -> Maybe URI
getURL (Blob _ url _) = Just url
getURL (Inner url _) = Just url
getURL _ = Nothing

-- |Gets the error from a Failure.
getError :: FetchResult e i -> Maybe e
getError (Failure e _) = Just e
getError _ = Nothing

-- |Constructs a 'Blob' without an identifier.
blob :: URI -> (Request -> Request) -> FetchResult e i
blob = Blob Nothing

-- |Constructs a 'PlainText' without an identifier.
plainText :: Text -> FetchResult e i
plainText = PlainText Nothing

-- |Constructs a 'BinaryData' without an identifier.
binaryData :: ByteString -> FetchResult e i
binaryData = BinaryData Nothing

-- |Constructs an 'XmlResult' without an identifier.
xmlResult :: XmlTree -> FetchResult e i
xmlResult = XmlResult Nothing

-- |Constructs an 'Info' without an identifier.
info :: Text -> Text -> FetchResult e i
info = Info Nothing

-- |Constructs a failure nodes, wrapping the error into 'SomeException'.
failure :: Exception e => e -> Maybe (FetchResult SomeException i, Maybe FilePath) -> FetchResult SomeException i
failure e = Failure (SomeException e)

instance (Show i, Show e) => Show (FetchResult e i) where
   show (Blob i uri _) = "Blob " LS.++ show i LS.++ " " LS.++ show uri
   show (Inner uri _) = "Inner" LS.++ " " LS.++ show uri
   show (PlainText i t) = "PlainText " LS.++ show i LS.++ " " LS.++ show t
   show (BinaryData i t) = "BinaryData " LS.++ show i LS.++ " " LS.++ show t
   show (XmlResult i t) = "XmlResult " LS.++ show i LS.++ " "LS.++ show t
   show (Failure e t) = "Failure " LS.++ show e LS.++ " " LS.++ show t
   show (Info i k v) = "Info " LS.++ show i LS.++ " " LS.++ show k LS.++ " " LS.++ show v

-- |Types which can be saved to disk and have an associated file extension.
class HasExt a where
   -- |Returns the extension of a given value.
   ext :: a -> Text

-- |The file extension associated with a specific 'FetchResult'.
--  The values are:
--
--  * @.bin@ for 'Blob',
--  * @.txt@ for 'PlainText',
--  * @.xml@ for 'XmlResult',
--  * @.bin@ for 'BinaryData',
--  * @.info@ for 'Info'.
--  * @.error@ for 'Failure',
--  * @.inner@ for 'Inner'.
instance HasExt (FetchResult e i) where
   ext Blob{} = "blob"
   ext Inner{} = "inner"
   ext PlainText{} = "txt"
   ext BinaryData{} = "bin"
   ext XmlResult{} = "xml"
   ext Failure{} = "error"
   ext Info{} = "info"

-- |True iff the result is not of type 'Inner'.
isLeaf :: FetchResult e i -> Bool
isLeaf Inner{} = False
isLeaf _ = True

instance Ord XNode where
   compare (XText s) (XText t) = compare s t
   compare (XBlob s) (XBlob t) = compare s t
   compare (XCharRef s) (XCharRef t) = compare s t
   compare (XEntityRef s) (XEntityRef t) = compare s t
   compare (XCmt s) (XCmt t) = compare s t
   compare (XCdata s) (XCdata t) = compare s t
   compare (XPi s s') (XPi t t') = lex [compare s t, compare s' t']
   compare (XTag s s') (XTag t t') = lex [compare s t, compare s' t']
   compare (XDTD s s') (XDTD t t') = lex [compare s t, compare s' t']
   compare (XAttr s) (XAttr t) = compare s t
   compare (XError s s') (XError t t') = lex [compare s t, compare s' t']
   compare s t = compare (pos s) (pos t)
      where
         pos (XText _) = 0
         pos (XBlob _) = 1
         pos (XCharRef _) = 2
         pos (XEntityRef _) = 3
         pos (XCmt _) = 4
         pos (XCdata _) = 5
         pos (XPi _ _) = 6
         pos (XTag _ _) = 7
         pos (XDTD _ _) = 8
         pos (XAttr _) = 9
         pos (XError _ _) = 10

instance (Ord i, Ord e) => Eq (FetchResult e i) where
   x == y = compare x y == EQ

instance (Ord i, Ord e) => Ord (FetchResult e i) where
   compare (Blob i s _) (Blob j t _) = lex [compare i j, compare s t]
   compare (Inner s _) (Inner t _) = compare s t
   compare (PlainText i s) (PlainText j t) = lex [compare i j, compare s t]
   compare (BinaryData i s) (BinaryData j t) = lex [compare i j, compare s t]
   compare (XmlResult i s) (XmlResult j t) = lex [compare i j, compare s t]
   compare (Failure s s') (Failure t t') = lex [compare s t, compare s' t']
   compare (Info i k v) (Info j k' v') = lex [compare i j, compare k k', compare v v']
   compare s t = compare (pos s) (pos t)
      where
         pos :: FetchResult e i -> Int
         pos Blob{} = 0
         pos Inner{} = 1
         pos PlainText{} = 2
         pos BinaryData{} = 3
         pos XmlResult{} = 4
         pos Failure{} = 5
         pos Info{} = 6

-- |Automatically creates a failure node if the result
--  set is empty. This is useful for when at least 1 result
--  is expected.
noneAsFailure :: e -- ^The error to create.
              -> Maybe (FetchResult e i) -- ^The original node.
              -> [FetchResult e i] -- ^The result set  @S@ to check for emptiness.
              -> [FetchResult e i] -- ^@S@ if @not.null $ S@, @[f]@
                                   --  otherwise (for a new 'Failure' @f@).
noneAsFailure e b [] = [Failure e $ b >$> (,Nothing)]
noneAsFailure _ _ (x:xs) = x:xs

-- |Returns True iff the result is a Blob.
isBlob :: FetchResult e i -> Bool
isBlob Blob{} = True
isBlob _ = False

-- |Returns True iff the result is an inner node.
isInner :: FetchResult e i -> Bool
isInner Inner{} = True
isInner _ = False

-- |Returns True iff the result is binary data.
isBinaryData :: FetchResult e i -> Bool
isBinaryData BinaryData{} = True
isBinaryData _ = False

-- |Returns True iff the result is plain text.
isPlainText :: FetchResult e i -> Bool
isPlainText PlainText{} = True
isPlainText _ = False

-- |Returns True iff the result is an XML tree.
isXmlResult :: FetchResult e i -> Bool
isXmlResult XmlResult{} = True
isXmlResult _ = False

-- |Returns True iff the result is a failure.
isFailure :: FetchResult e i -> Bool
isFailure Failure{} = True
isFailure _ = False

-- |Returns True iff the result is auxiliary information.
isInfo :: FetchResult e i -> Bool
isInfo Info{} = True
isInfo _ = False
