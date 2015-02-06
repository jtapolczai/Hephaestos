{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |Helper functions for building crawlers.
module Crawling.Hephaestos.Crawlers.Utils (
   module X,
   makeLink,
   htmlSuccessor,
   toDocument,
   getXPath,
   getText,
   getSingleText,
   ) where

import Prelude hiding ((++))

import Codec.Binary.UTF8.String (decode)
import Control.Arrow
import Control.Exception
import Control.Monad.Catch
import Data.ByteString.Lazy (ByteString, unpack)
import Data.Char
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.List.Split
import Data.Maybe (mapMaybe)
import Data.ListLike (ListLike(append), StringLike(fromString, toString))
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Void
import qualified Network.URI as N
import Numeric.Peano

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor

import Text.XML.HXT.DOM.TypeDefs as X
import Text.XML.HXT.Parser.HtmlParsec as X (parseHtmlContent)
import Text.XML.HXT.XPath.XPathDataTypes as X
import Text.XML.HXT.XPath.XPathEval as X hiding (getXPath)
import qualified Text.XML.HXT.XPath.XPathEval as XP (getXPath)

import Debug.Trace

-- |Tries to turn a text into an absolute link.
--  If a Text can parsed, then one two things will be done:
--
--  * if it is a relative link, it will be combined with the given URI into an
--    absolute one.
--  * if it is absolute, it is left as-is.
--
--  If a Text cannot be parsed, a Failure result with a 'DataFormatError'
--  will be created.
makeLink :: N.URI -- ^URI of the current page (for making relative links absolute).
         -> (N.URI -> (Request -> Request) -> FetchResult SomeException i)
         -- ^The FetchResult into which the links should be wrapped. Commonly 'Inner' or 'Blob'.
         -> T.Text
         -- ^The text which should be made into a link, if possible.
         -> FetchResult SomeException i
makeLink uri f u =
   maybe (failure (URIParsingError $ fromString $ show uri) Nothing)
         (flip f id . flip N.nonStrictRelativeTo uri)
         (N.parseURIReference $ T.unpack u)

-- |Constructs a general 'Successor' from a 'HTMLSuccessor'. If the input
--  cannot be parsed as HTML, a failure node is created.
htmlSuccessor :: (Request -> Request) -- ^The request modifier function.
                                      --  This is necessary for the creation
                                      --  of the failure node in case the input
                                      --  can't be parsed as HTML.
              -> HTMLSuccessor SomeException i a
              -> Successor SomeException i a
htmlSuccessor reqF succ uri bs st =
   bs >$> toDocument (fromString $ show uri) >>=
   \case (Right html) -> succ uri html st
         (Left err) -> return [SuccessorNode st (Failure err $ Just (Inner uri reqF, Nothing))]

-- |Tries to parse a ByteString as a HTML document, throwing
--  a 'HTMLParsingError' on failure.
toDocument :: MonadThrow m => URL -> ByteString -> m XmlTree
toDocument u = res . root . parseHtmlContent . decode . unpack
   where
      root = filter (maybe False ("html"==) . getTag . unTree)

      res [] = throwM $ HTMLParsingError u
      res (x:_) = return x

      getTag (XTag x _) = Just $ localPart x
      getTag _          = Nothing

-- |Executes an XPath expression on a document.
--  See 'Text.XML.HXT.XPath.XPathEval.getXPath'.
getXPath :: StringLike full => full -> XmlTree -> XmlTrees
getXPath = XP.getXPath . toString

-- |Extracts the node value from an 'NTree'.
unTree :: NTree a -> a
unTree (NTree a _) = a

-- |Extracts the textual content from an 'XNode' if
--  doing so doesn't result in loss of information.
--  This is a convenience function for
--  situations in which one isn't much interested in
--  the type of the content.
--
--  Specifically, it extracts:
--
--  * The text from 'XText', 'XCmt', 'XEntityRef', and 'XCdata';
--  * the Blob from 'XBlob', converted from 'ByteString'
--    to 'Text';
--  * the Int from 'XCharRef' via show;
--  * the getText, applied recursively to the contents of 'XTag';
--  * the value from 'XAttr'.
--
--  Note that Blobs are decoded using UTF8.
--
--  The following nodes return 'Nothing':
--
--  * 'XTag',
--  * 'XPi',
--  * 'XDTD',
--  * 'XError'.
getText :: XmlTree -> Maybe T.Text
getText (NTree (XText x) _) = Just $ T.pack x
getText (NTree (XBlob x) _) = Just $ decodeUtf8 x
getText (NTree (XCharRef x) _) = Just $ T.pack $ show x
getText (NTree (XEntityRef x) _) = Just $ T.pack x
getText (NTree (XCmt x) _) = Just $ T.pack x
getText (NTree (XCdata x) _) = Just $ T.pack x
getText (NTree (XTag _ _) ns) = Just $ T.concat $ mapMaybe getText ns
getText (NTree (XAttr _) ns) = Just $ T.concat $ mapMaybe getText ns
getText _ = Nothing

-- |Applies 'getText' to all nodes in a list
--  and concatenates the non-null results.
--  If there are no results, the particular monad's error case is returned.
getSingleText :: MonadThrow m => XmlTrees -> m T.Text
getSingleText = ifN . T.concat . mapMaybe getText
   where
      ifN x | T.null x  = throwM $ DataMissingError "" Nothing
            | otherwise = return x
