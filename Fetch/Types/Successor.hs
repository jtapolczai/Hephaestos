-- |Contains the 'Successor' type which is used to
--  construct crawl trees, together with a set of useful
--  helper functions for creating 'Successor' functions.
--
--  For examples of successor functions, see @Galleries.Examples@.
module Fetch.Types.Successor (
   -- *Types
   Successor,
   FetchResult(..),
   -- *Helper functions relating to state and failure
   mapState,
   mapVoid,
   noneAsFailure,
   noneAsDataFailure,
   -- *Discriminator functions
   isBlob,
   isPlainText,
   isXmlResult,
   isFailure,
   isInfo,
   asBlob,
   asPlainText,
   asXmlResult,
   asInfo,
   )where

import Control.Arrow
import Data.Text
import Data.Void
import Fetch.Types
import Text.XML.HXT.DOM.TypeDefs

-- |A function which extracts a number of successor nodes
--  from a page. The first component of the result is the
--  lift of leaves (i.e. nodes which should not be expanded
--  further) and the second is the list of nudes which should
--  be expanded.
--  The third component is auxiliary information which
--  should be passed on to possible recursive calls
--  of 'Successor'.
type Successor a e = URL
                     -> XmlTree
                     -> a
                     -> ([FetchResult a e], [(FetchResult a e, a)])

-- |Result of a fetching operation.
data FetchResult a e =
   -- |A URL which to download.
   Blob{fromBlob::URL}
   -- |Some plain text without any deeper semantic value.
   | PlainText{fromPlainText::Text}
   -- |An XML tree.
   | XmlResult{fromXmlResult::XmlTree}
   -- |A failure which stores the error which occurred,
   --  together with everything needed to retry the fetch.
   | Failure{failureState::a,failureURL::URL, failureError::e}
   -- |A piece of named auxiliary information, such as a title or an author.
   | Info{infoKey::Text,infoValue::Text}

-- |Uniformly adds a new state to the result of a 'Successor' function.
mapState :: Functor f => b -> f a -> f (a,b)
mapState x = fmap (id &&& const x)

-- |Uniformly adds a 'Void' state to the result of a 'Successor' function.
mapVoid :: Functor f => f a -> f (a,Void)
mapVoid = mapState undefined

-- |Convenience function which turns a collection of URLs
--  into Blob FetchResults.
asBlob :: Functor f => f URL -> f (FetchResult a e)
asBlob = fmap Blob

-- |Convenience function which turns a collection of Texts
--  into PlainText FetchResults.
asPlainText :: Functor f => f Text -> f (FetchResult a e)
asPlainText = fmap PlainText

-- |Convenience function which turns a collection of XmlTrees
--  into XmlResult FetchResults.
asXmlResult :: Functor f => f XmlTree -> f (FetchResult a e)
asXmlResult = fmap XmlResult

-- |Convenience function which turns a collection of tuples
--  into Info FetchResults.
asInfo :: Functor f => f (Text,Text) -> f (FetchResult a e)
asInfo = fmap (uncurry Info)

-- |Automatically creates a failure node if the result
--  set is empty. This is useful for when at least 1 result
--  is expected.
noneAsFailure :: a -- ^The input state of the 'Successor' function.
              -> e -- ^The error to create.
              -> URL -- ^The input URL of the 'Successor' function.
              -> [FetchResult a e] -- ^The result set  @S@ to check for emptiness.
              -> [FetchResult a e] -- ^@S@ if @not.null $ S@, @[f]@
                                        --  otherwise (for a new 'Failure' @f@).
noneAsFailure a e url [] = [Failure a url e]
noneAsFailure _ _ _ (x:xs) = x:xs

-- |Simpler version of 'noneAsFailure' which creates
--  a 'DataFindingError' with a default error message.
noneAsDataFailure :: a
                  -> URL
                  -> [FetchResult a NetworkError]
                  -> [FetchResult a NetworkError]
noneAsDataFailure a url = noneAsFailure a (dataFindingError url) url

-- |Returns True iff the result is a Blob.
isBlob :: FetchResult a e -> Bool
isBlob Blob{} = True
isBlob _ = False

-- |Returns True iff the result is plain text.
isPlainText :: FetchResult a e -> Bool
isPlainText PlainText{} = True
isPlainText _ = False

-- |Returns True iff the result is an XML tree.
isXmlResult :: FetchResult a e -> Bool
isXmlResult XmlResult{} = True
isXmlResult _ = False

-- |Returns True iff the result is a failure.
isFailure :: FetchResult a e -> Bool
isFailure Failure{} = True
isFailure _ = False

-- |Returns True iff the result is auxiliary information.
isInfo :: FetchResult a e -> Bool
isInfo Info{} = True
isInfo _ = False
