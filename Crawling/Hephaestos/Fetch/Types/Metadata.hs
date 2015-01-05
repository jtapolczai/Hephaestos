{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Contains types for metadata that is stores during a fetch process.
module Crawling.Hephaestos.Fetch.Types.Metadata where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Functor.Monadic
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, isNothing)
import Data.Text.Lazy
import Data.Tree

import Crawling.Hephaestos.Fetch.Types
import qualified Crawling.Hephaestos.Fetch.Types.Successor as S

-- |A metadata node.
data MetaNode = InnerNode{metaURL::URL}
                | Leaf{metaURL::URL, metaFile::Text, metaType::ResultType}

-- |The type of a FetchResult
data ResultType = Blob | Inner | PlainText | XmlResult | BinaryData | Failure | Info
   deriving (Show, Eq, Ord, Read, Enum)

-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON MetaNode where
   parseJSON (Object v) = do
      url <- v .: "url"
      file <- v .:? "file"
      typ <- v .:? "type"
      return $ if isNothing typ || typ == Just Inner
         then InnerNode url
         else Leaf url (fromJust file) (fromJust typ)
   parseJSON _ = mzero

instance FromJSON ResultType where
   parseJSON (String s) = case toLower $ fromStrict s of
      "blob" -> return Blob
      "inner" -> return Inner
      "plaintext" -> return PlainText
      "xmlresult" -> return XmlResult
      "binarydata" -> return BinaryData
      "failure" -> return Failure
      "info" -> return Info
   parseJSON _ = mzero

instance ToJSON MetaNode where
   toJSON (InnerNode url) = object ["url" .= url]
   toJSON (Leaf url file typ) = object ["url" .= url, "file" .= file, "type" .= typ]

instance ToJSON ResultType where
   toJSON Blob = String "Blob"
   toJSON Inner = String "Inner"
   toJSON PlainText = String "PlainText"
   toJSON XmlResult = String "XmlResult"
   toJSON BinaryData = String "BinaryData"
   toJSON Failure = String "Failure"
   toJSON Info = String "Info"

-- Helpers
-------------------------------------------------------------------------------

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
