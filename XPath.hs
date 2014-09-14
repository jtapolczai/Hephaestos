{-# LANGUAGE OverloadedStrings #-}

-- |Wrapper around 'Text.XML' and 'Text.XML.Cursor', with some helper functions
--  added. Importing this module should be sufficient for all XPath-related
--  needs.
module XPath (
   module X,
   toDocument,
   getXPath,
   getXPathLeaves,
   getText,
   getSingleText,
   unTree,
   )where

import Codec.Binary.UTF8.String (decode)
import qualified Data.ByteString as B (concat)
import Data.ByteString.Lazy (ByteString, unpack, toChunks)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Text.XML.HXT.XPath.XPathEval as XP (getXPath)
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.DOM.TypeDefs as X
import Text.XML.HXT.Parser.HtmlParsec as X (parseHtmlContent)
import Text.XML.HXT.XPath.XPathDataTypes as X
import Text.XML.HXT.XPath.XPathEval as X hiding (getXPath)

import Fetch.ErrorHandling
import Fetch.Types

-- |Runs an XPath expression against a document tree.
-- runXPath :: ByteString -> (String -> a) -> a
--runXPath doc expr = expr (decode $ unpack doc)

toDocument :: URL -> ByteString -> ErrorIO XmlTree
toDocument u = res . root . parseHtmlContent . decode . unpack
   where
      root = filter (maybe False ("html"==) . getTag . unTree)

      res [] = addNetworkError u HTMLParsingError
      res (x:_) = return x

      getTag (XTag x _) = Just $ localPart x
      getTag _        = Nothing

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
getXPathLeaves :: T.Text -> XmlTree -> [XNode]
getXPathLeaves t d = map unTree $ getXPath t d

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

--  * The text from 'XText', 'XCmt', 'XEntityRef', and 'XCdata';
--  * the Blob from 'XBlob', converted from 'ByteString'
--    to 'Text';
--  * the Int from 'XCharRef' via show;
--  * the fully qualified name from 'XTag';
--  * the fully qualified name from 'XAttr'.
--
--  Note that Blobs are decoded using UTF8.
--  
--  The following nodes return 'Nothing':
--
--  * 'XTag',
--  * 'XPi',
--  * 'XDTD',
--  * 'XError'.
getText :: XNode -> Maybe T.Text
getText (XText x) = Just $ T.pack x
getText (XBlob x) = Just $ decodeUtf8 $ B.concat $ toChunks x
getText (XCharRef x) = Just $ T.pack $ show x
getText (XEntityRef x) = Just $ T.pack x
getText (XCmt x) = Just $ T.pack x
getText (XCdata x) = Just $ T.pack x
getText (XAttr x) = Just $ T.pack $ qualifiedName x
getText _ = Nothing


-- |Applies 'getText' to all nodes in a list
--  and concatenates the non-null results.
--  If concatenated results are non-empty (length > 1),
--  they are returned as a single-element list.
--  Otherwise, the empty list is returned.
getSingleText :: [XNode] -> [T.Text]
getSingleText = ifN . T.concat . mapMaybe getText
   where
      ifN x | T.null x  = []
            | otherwise = [x]