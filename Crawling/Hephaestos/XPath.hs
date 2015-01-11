{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Wrapper around 'Text.XML' and 'Text.XML.Cursor', with some helper functions
--  added. Importing this module should be sufficient for all XPath-related
--  needs.
module Crawling.Hephaestos.XPath (
   module X,
   toDocument,
   getXPath,
   getXPathLeaves,
   getText,
   getSingleText,
   unTree,
   )where

import Codec.Binary.UTF8.String (decode)
import Control.Exception
import Control.Monad.Except
import qualified Data.ByteString as B (concat)
import Data.ByteString.Lazy (ByteString, unpack, toChunks)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Text.XML.HXT.XPath.XPathEval as XP (getXPath)
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.DOM.TypeDefs as X
import Text.XML.HXT.Parser.HtmlParsec as X (parseHtmlContent)
import Text.XML.HXT.XPath.XPathDataTypes as X
import Text.XML.HXT.XPath.XPathEval as X hiding (getXPath)

import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Types

-- |Tries to parse a ByteString as a HTML document, throwing
--  a 'HTMLParsingError' on failure.
toDocument :: MonadError SomeException m => URL -> ByteString -> m XmlTree
toDocument u = res . root . parseHtmlContent . decode . unpack
   where
      root = filter (maybe False ("html"==) . getTag . unTree)

      res [] = throwError $ htmlParsingError u
      res (x:_) = return x

      getTag (XTag x _) = Just $ localPart x
      getTag _          = Nothing

-- |Executes an XPath expression on a document.
--  See 'Text.XML.HXT.XPath.XPathEval.getXPath'.
getXPath :: T.Text -> XmlTree -> XmlTrees
getXPath = XP.getXPath . T.unpack

-- |Simplified version of 'getXPath' for when
--  one only wishes to extract simple values, not
--  entire XML trees. Only the roots of the found
--  XML trees are taken and their children (presumed to be
--  non-existent) are discarded.
--
--  Defined as @getXPathLeaves t d = map unTree $ getXPath t d@.
getXPathLeaves :: T.Text -> XmlTree -> XmlTrees
getXPathLeaves t d = getXPath t d

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
--  If the concatenated results are non-empty (length >= 1),
--  they are returned as a single-element list.
--  Otherwise, the empty list is returned.
getSingleText ::XmlTrees -> [T.Text]
getSingleText = ifN . T.concat . mapMaybe getText
   where
      ifN x | T.null x  = []
            | otherwise = [x]
