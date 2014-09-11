{-# LANGUAGE OverloadedStrings #-}

-- |Wrapper around 'Text.XML' and 'Text.XML.Cursor', with some helper functions
--  added. Importing this module should be sufficient for all XPath-related
--  needs.
module XPath (
   module X,
   toDocument,
   concatText,
   getText,
   getAttr,
   getTag,
   unTree,
   )where

import Codec.Binary.UTF8.String (decode)
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.DOM.TypeDefs as X
import Text.XML.HXT.Parser.HtmlParsec as X (parseHtmlContent)
import Text.XML.HXT.XPath.XPathDataTypes as X
import Text.XML.HXT.XPath.XPathEval as X
import Data.ByteString.Lazy (ByteString, unpack)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T (concat, pack, Text)

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

-- |Extracts the content of all 'XText'-nodes and concatenates the
--  results. Other nodes are discarded.
concatText :: XmlTrees -> T.Text
concatText = T.concat . mapMaybe (getText . unTree)

-- |A named destructor for 'XText'.
getText :: XNode -> Maybe T.Text
getText (XText x) = Just $ T.pack x
getText _         = Nothing

-- |A named destructor for 'XAttr'.
getAttr :: XNode -> Maybe T.Text
getAttr (XAttr x) = Just $ T.pack $ localPart x

-- |A named destructor for 'XTag'.
getTag :: XNode -> Maybe T.Text
getTag (XTag x _) = Just $ T.pack $ localPart x
getTag _        = Nothing

unTree :: NTree a -> a
unTree (NTree a _) = a
