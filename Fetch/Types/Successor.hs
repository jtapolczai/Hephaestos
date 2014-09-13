-- |Contains the 'Successor' type which is used to
--  construct crawl trees.
module Fetch.Types.Successor (
   -- *Types
   Successor,
   FetchResult(..),
   -- *Helper functions
   mapState,
   mapVoid,
   -- *Discriminator functions,
   isBlob,
   isPlainText,
   isXmlResult,
   isFailure,
   isInfo,
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
   -- ^A URL which to download.
   Blob{fromBlob::URL}
   -- ^Some plain text without any deeper semantic value.
   | PlainText{fromPlainText::Text}
   -- ^An XML tree.
   | XmlResult{fromXmlResult::XmlTree}
   -- ^A failure which stores the error which occurred,
   --  together with everything needed to retry the fetch.
   | Failure{failureState::a,failureURL::URL, failureError::e}
   -- ^A piece of named auxiliary information, such as a title or an author.
   | Info{infoKey::Text,infoValue::Text}

-- |Uniformly adds a new state to the result of a 'Successor' function.
mapState :: Functor f => b -> f a -> f (a,b)
mapState x = fmap (id &&& const x)

-- |Uniformly adds a 'Void' state to the result of a 'Successor' function.
mapVoid :: Functor f => f a -> f (a,Void)
mapVoid = mapState undefined

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
