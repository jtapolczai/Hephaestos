-- |Contains types for metadata that is stores during a fetch process.
module Crawling.Hephaestos.Fetch.Types.Metadata where

import Data.Text.Lazy

import Crawling.Hephaestos.Fetch.Types
import qualified Crawling.Hephaestos.Fetch.Types.Successor as S

-- |A metadata node.
data MetaNode = InnerNode{metaURL::URL}
                | Leaf{metaURL::URL, metaFile::Text, metaType::ResultType}

-- |The type of a FetchResult
data ResultType = Blob | Inner | PlainText | XmlResult | BinaryData | Failure | Info

-- |Gets the type of a FetchResult
getType :: S.FetchResult e -> ResultType
getType S.Blob = Blob
getType S.Inner = Inner
getType (S.PlainText _) = PlainText
getType (S.XmlResult _) = XmlResult
getType (S.BinaryData _) = BinaryData
getType (S.Failure _ _) = Failure
getType (S.Info _ _) = Info

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
