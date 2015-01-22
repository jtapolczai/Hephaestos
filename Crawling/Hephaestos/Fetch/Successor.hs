{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Contains the 'Successor' type which is used to
--  construct crawl trees, together with a set of useful
--  helper functions for creating 'Successor' functions.
--
--  For examples of successor functions, see @Galleries.Examples@.
module Crawling.Hephaestos.Fetch.Successor (
   -- *Types
   Successor,
   HTMLSuccessor,
   FetchResult(..),
   failure,
   isLeaf,
   SuccessorNode(..),
   SuccessorNode',
   HasExt(..),
   -- *Helper functions relating to state and failure
   simpleNode,
   voidNode,
   noneAsFailure,
   -- *Discriminator functions
   isBlob,
   isInner,
   isBinaryData,
   isPlainText,
   isXmlResult,
   isFailure,
   isInfo,
   asBinaryData,
   asPlainText,
   asXmlResult,
   asInfo,
   -- * Adding HTTP headers
   addHeader,
   ) where

import Prelude hiding (lex, FilePath)

import Control.Arrow
import Control.Exception
import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Functor.Monadic
import qualified Data.List.Safe as LS
import qualified Data.ListLike as LI
import Data.Text.Lazy hiding (pack, toStrict)
import Data.Types.Injective
import Data.Void
import Filesystem.Path.CurrentOS
import Network.HTTP.Conduit (Request, requestHeaders)
import Network.HTTP.Types.Header (HeaderName)
import Network.URI (URI)
import Text.XML.HXT.DOM.TypeDefs

import Crawling.Hephaestos.Fetch.Types

-- |A function which extracts a number of successor nodes from a page.
type Successor e a = URI -- ^The URI of the input
                     -> ByteString -- ^The input as a byte string.
                     -> a -- ^The input state.
                     -> [SuccessorNode e a]
                        -- ^The list of resultant leaves and nodes.

-- |A Successor which only works on HTML input. See 'htmlSuccessor'.
type HTMLSuccessor e a = URI
                         -> XmlTree
                         -> a
                         -> [SuccessorNode e a]

-- |A node generated by a 'Successor' function.
--  See 'simpleNode', 'voidNode' and 'redNode' for smart constructors.
data SuccessorNode e a = SuccessorNode {nodeState::a,
                                        -- ^The new state.
                                        nodeRes::FetchResult e
                                        -- ^The node's result.
                                       }

-- |Shorthand for @SuccessorNode SomeException@
type SuccessorNode' a = SuccessorNode SomeException a

-- |Functor instance over the node state.
instance Functor (SuccessorNode e) where
   fmap f s@SuccessorNode{nodeState=a} = s{nodeState=f a}

-- |Selects the first non-EQ element from a list or EQ if no such element exists.
lex :: [Ordering] -> Ordering
lex = maybe EQ id . LS.head . LS.dropWhile (EQ==)

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


-- |Creates a 'SuccessorNode' from a 'FetchResult' and a state. No request
--  modifiers will be applied.
simpleNode :: a -> FetchResult e -> SuccessorNode e a
simpleNode s r = SuccessorNode s r

-- |Creates a 'SuccessorNode' from a 'FetchResult'.
--  No request modifiers will be applied.
voidNode :: FetchResult e -> SuccessorNode e Void
voidNode = simpleNode undefined

-- |Result of a fetching operation.
data FetchResult e =
   -- |A URL which is to be downloaded.
   Blob{blobURL::URI, reqMod::Request -> Request}
   -- |An inner node in a search treee.
   | Inner{innerURL::URI, reqMod::Request -> Request}
   -- |Some plain text without any deeper semantic value.
   | PlainText{fromPlainText::Text}
   -- |A ByteString (binary data).
   | BinaryData{fromBinary::ByteString}
   -- |An XML tree.
   | XmlResult{fromXmlResult::XmlTree}
   -- |A failure which stores an error and the original node, if present.
   | Failure{failureError::e,
             -- ^The error which occurred.
             originalNode::Maybe (FetchResult e, Maybe FilePath)
             -- ^If applicable, the original node which couldn't be saved.
             --  This is most useful in the case of 'Blob's.
            }
   -- |A piece of named auxiliary information, such as a title or an author.
   | Info{infoKey::Text,infoValue::Text}

-- |Constructs a failure nodes, wrapping the error into 'SomeException'.
failure :: Exception e => e -> Maybe (FetchResult SomeException, Maybe FilePath) -> FetchResult SomeException
failure e m = Failure (SomeException e) m

instance Show e => Show (FetchResult e) where
   show (Blob uri _) = "Blob " LS.++ show uri
   show (Inner uri _) = "Inner" LS.++ show uri
   show (PlainText t) = "PlainText " LS.++ show t
   show (BinaryData t) = "BinaryData " LS.++ show t
   show (XmlResult t) = "XmlResult " LS.++ show t
   show (Failure e t) = "Failure " LS.++ show e LS.++ " " LS.++ show t
   show (Info k v) = "Info " LS.++ show k LS.++ " " LS.++ show v

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
instance HasExt (FetchResult e) where
   ext Blob{} = "blob"
   ext Inner{} = "inner"
   ext PlainText{} = "txt"
   ext BinaryData{} = "bin"
   ext XmlResult{} = "xml"
   ext Failure{} = "error"
   ext Info{} = "info"

-- |True iff the result is not of type 'Inner'.
isLeaf :: FetchResult e -> Bool
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

instance Ord e => Eq (FetchResult e) where
   x == y = compare x y == EQ

instance (Ord e) => Ord (FetchResult e) where
   compare (Blob s _) (Blob t _) = compare s t
   compare (Inner s _) (Inner t _) = compare s t
   compare (PlainText s) (PlainText t) = compare s t
   compare (BinaryData s) (BinaryData t) = compare s t
   compare (XmlResult s) (XmlResult t) = compare s t
   compare (Failure s s') (Failure t t') = lex [compare s t, compare s' t']
   compare (Info k v) (Info k' v') = lex [compare k k', compare v v']
   compare s t = compare (pos s) (pos t)
      where
         pos :: FetchResult e -> Int
         pos Blob{} = 0
         pos Inner{} = 1
         pos PlainText{} = 2
         pos BinaryData{} = 3
         pos XmlResult{} = 4
         pos Failure{} = 5
         pos Info{} = 6

-- |Convenience function which turns a collection of ByteStrings into
--  BinaryData FetchResults.
asBinaryData :: Functor f => f ByteString -> f (FetchResult e)
asBinaryData = fmap BinaryData

-- |Convenience function which turns a collection of Texts
--  into PlainText FetchResults.
asPlainText :: Functor f => f Text -> f (FetchResult e)
asPlainText = fmap PlainText

-- |Convenience function which turns a collection of XmlTrees
--  into XmlResult FetchResults.
asXmlResult :: Functor f => f XmlTree -> f (FetchResult e)
asXmlResult = fmap XmlResult

-- |Convenience function which turns a collection of tuples
--  into Info FetchResults.
asInfo :: Functor f => f (Text,Text) -> f (FetchResult e)
asInfo = fmap (uncurry Info)

-- |Automatically creates a failure node if the result
--  set is empty. This is useful for when at least 1 result
--  is expected.
noneAsFailure :: e -- ^The error to create.
              -> Maybe (FetchResult e) -- ^The original node.
              -> [FetchResult e] -- ^The result set  @S@ to check for emptiness.
              -> [FetchResult e] -- ^@S@ if @not.null $ S@, @[f]@
                                 --  otherwise (for a new 'Failure' @f@).
noneAsFailure e b [] = [Failure e $ b >$> (,Nothing)]
noneAsFailure _ _ (x:xs) = x:xs

-- |Returns True iff the result is a Blob.
isBlob :: FetchResult e -> Bool
isBlob Blob{} = True
isBlob _ = False

-- |Returns True iff the result is an inner node.
isInner :: FetchResult e -> Bool
isInner Inner{} = True
isInner _ = False

-- |Returns True iff the result is binary data.
isBinaryData BinaryData{} = True
isBinaryData _ = False

-- |Returns True iff the result is plain text.
isPlainText :: FetchResult e -> Bool
isPlainText PlainText{} = True
isPlainText _ = False

-- |Returns True iff the result is an XML tree.
isXmlResult :: FetchResult e -> Bool
isXmlResult XmlResult{} = True
isXmlResult _ = False

-- |Returns True iff the result is a failure.
isFailure :: FetchResult e -> Bool
isFailure Failure{} = True
isFailure _ = False

-- |Returns True iff the result is auxiliary information.
isInfo :: FetchResult e -> Bool
isInfo Info{} = True
isInfo _ = False
