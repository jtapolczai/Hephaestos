{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Contains the 'Successor' type which is used to
--  construct crawl trees, together with a set of useful
--  helper functions for creating 'Successor' functions.
--
--  For examples of successor functions, see @Galleries.Examples@.
module Crawling.Hephaestos.Fetch.Types.Successor (
   -- *Types
   Successor,
   HTMLSuccessor,
   htmlSuccessor,
   FetchResult(..),
   typeExt,
   isLeaf,
   SuccessorNode(..),
   SuccessorNode',
   -- *Helper functions relating to state and failure
   simpleNode,
   voidNode,
   reqNode,
   noneAsFailure,
   noneAsDataFailure,
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
   ) where

import Prelude hiding (lex)

import Control.Arrow
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.List.Safe as LS
import Data.Text.Lazy
import Data.Void
import Network.HTTP.Conduit (Request)
import Network.URI (URI)
import Text.XML.HXT.DOM.TypeDefs

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Helper.String (showT)
import Crawling.Hephaestos.XPath

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
                                        nodeRes::FetchResult e,
                                        -- ^The node's result. Only 'Blob's will
                                        --  be expanded.
                                        nodeReqMod::Request -> Request,
                                        -- ^Modifiers for the next HTTP request.
                                        nodeURL :: URL
                                        -- ^This node's URL
                                       }

-- |Shorthand for @SuccessorNode SomeException@
type SuccessorNode' a = SuccessorNode SomeException a

-- |Functor instance over the node state.
instance Functor (SuccessorNode e) where
   fmap f s@SuccessorNode{nodeState=a} = s{nodeState=f a}

-- |Eq instance for nodes based solely on the URL and the node state.
instance (Eq a) => Eq (SuccessorNode e a) where
   n == m = (nodeURL n == nodeURL m) &&
            (nodeState n   == nodeState m)

-- |Ord instance for nodes. As with Eq, onl< the URL and the node state
--  are compared (in this lexical order),
--  not the request modifier function.
instance (Ord a) => Ord (SuccessorNode e a) where
   compare n m = lex [compare (nodeURL n) (nodeURL m),
                      compare (nodeState n) (nodeState m)]

-- |Selects the first non-EQ element from a list or EQ if no such element exists.
lex :: [Ordering] -> Ordering
lex = maybe EQ id . LS.head . LS.dropWhile (EQ==)

-- |Constructs a general 'Successor' from a 'HTMLSuccessor'. If the input
--  cannot be parsed as HTML, a failure node is created.
htmlSuccessor :: (Request -> Request) -- ^The request modifier function.
                                      --  This is necessary for the creation
                                      --  of the failure node in case the input
                                      --  can't be parsed as HTML.
              -> HTMLSuccessor SomeException a
              -> Successor SomeException a
htmlSuccessor reqF succ uri bs st =
   case toDocument (showT uri) bs of
      (Right html) -> succ uri html st
      (Left err) -> [SuccessorNode st (Failure err $ Just Inner) reqF (showT uri)]

-- |Creates a 'SuccessorNode' from a 'FetchResult' and a state. No request
--  modifiers will be applied.
simpleNode :: a -> FetchResult e -> URL -> SuccessorNode e a
simpleNode s r = SuccessorNode s r id

-- |Creates a 'SuccessorNode' from a 'FetchResult'.
--  No request modifiers will be applied.
voidNode :: FetchResult e -> URL -> SuccessorNode e Void
voidNode = simpleNode undefined

-- |Creates a 'SuccessorNode' from a 'FetchResult' and a request modifier
--  function.
reqNode :: FetchResult e -> (Request -> Request) -> URL -> SuccessorNode e Void
reqNode = SuccessorNode undefined

-- |Result of a fetching operation.
data FetchResult e =
   -- |A URL which is to be downloaded.
   Blob
   -- |An inner node in a search treee.
   | Inner
   -- |Some plain text without any deeper semantic value.
   | PlainText{fromPlainText::Text}
   -- |A ByteString (binary data).
   | BinaryData{fromBinary::ByteString}
   -- |An XML tree.
   | XmlResult{fromXmlResult::XmlTree}
   -- |A failure which stores an error and the original node, if present.
   --  Storing the original node allows the re-trying the node;
   --  otherwise, a failure just represents a general, non-corrigible error.
   | Failure{failureError::e, originalNode::Maybe (FetchResult e)}
   -- |A piece of named auxiliary information, such as a title or an author.
   | Info{infoKey::Text,infoValue::Text}
   deriving (Show, Eq)

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
typeExt :: FetchResult e -> Text
typeExt Blob = ".bin"
typeExt Inner = ".inner"
typeExt PlainText{} = ".txt"
typeExt BinaryData{} = ".bin"
typeExt XmlResult{} = ".xml"
typeExt Failure{} = ".error"
typeExt Info{} = ".info"

-- |True iff the result is not of type 'Inner'.
isLeaf :: FetchResult e -> Bool
isLeaf Inner = False
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

instance (Ord e) => Ord (FetchResult e) where
   compare Blob Blob = EQ
   compare Inner Inner = EQ
   compare (PlainText s) (PlainText t) = compare s t
   compare (BinaryData s) (BinaryData t) = compare s t
   compare (XmlResult s) (XmlResult t) = compare s t
   compare (Failure s s') (Failure t t') = lex [compare s t, compare s' t']
   compare (Info k v) (Info k' v') = lex [compare k k', compare v v']
   compare s t = compare (pos s) (pos t)
      where
         pos :: FetchResult e -> Int
         pos Blob = 0
         pos Inner = 1
         pos (PlainText _) = 2
         pos (BinaryData _) = 3
         pos (XmlResult _) = 4
         pos (Failure _ _) = 5
         pos (Info _ _) = 6

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
noneAsFailure e b [] = [Failure e b]
noneAsFailure _ _ (x:xs) = x:xs

-- |Simpler version of 'noneAsFailure' which creates
--  a 'DataFindingError' with a default error message.
noneAsDataFailure :: URL
                  -> Maybe (FetchResult SomeException)
                  -> [FetchResult SomeException]
                  -> [FetchResult SomeException]
noneAsDataFailure url b = noneAsFailure
                          (SomeException $ dataFindingError url)
                          b

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
