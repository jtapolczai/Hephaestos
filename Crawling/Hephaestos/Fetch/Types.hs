{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |The common types used by the other modules.
module Crawling.Hephaestos.Fetch.Types (
   module X,
   URL,
   WildcardURL,
   HTTPStatus,
   -- * Exceptions
   HTMLParsingError(..),
   DataMissingError(..),
   DataFormatError(..),
   DomainCrossedError(..),
   DuplicateFileError(..),
   AmbiguousDataError(..),
   dataMissingError,
   dataMissingError',
   dataFormatError,
   dataFormatError',
   duplicateFileError,
   -- * Configuration data
   FetchOptions(..),
   addReferer,
   manager,
   reqFunc,
   savePath,
   maxFailureNodes,
   ) where

import Prelude hiding (FilePath)

import Control.Exception
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Lazy hiding (fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable
import Filesystem.Path.CurrentOS (FilePath)
import Network.HTTP.Conduit as X (Request, Manager, HttpException(..))
import qualified Network.HTTP.Types as Ty
import Text.XML.HXT.DOM.TypeDefs


-- |Configuration data for fetch processes.
--  This record represents global options that a complex fetching function
--  might take into account.
data FetchOptions = FetchOptions {_addReferer :: Bool,
                                  _manager :: Manager,
                                  _reqFunc :: Request -> Request,
                                  _savePath :: FilePath,
                                  _maxFailureNodes :: Maybe Int}

makeLenses ''FetchOptions

-- |A URL.
type URL = Text
-- |A URL with possible wildcards (*) in it.
type WildcardURL = Text

-- |A numerical HTTP response status.
type HTTPStatus = Int

-- |The content of a page could not be parsed as HTML.
data HTMLParsingError = HTMLParsingError URL deriving (Show, Eq, Read, Typeable)
-- |A piece of expected data was missing on a page.
data DataMissingError = DataMissingError URL (Maybe Text) deriving (Show, Eq, Read, Typeable)
-- |Data was not in the expected format.
data DataFormatError = DataFormatError URL (Maybe Text) deriving (Show, Eq, Read, Typeable)
-- |A URL lay outside of an expected domain.
data DomainCrossedError = DomainCrossedError WildcardURL URL deriving (Show, Eq, Read, Typeable)
-- |A file with the given name already existed.
data DuplicateFileError = DuplicateFileError (Maybe Text) Text deriving (Show, Eq, Read, Typeable)
-- |More than one piece of suitable data was found.
data AmbiguousDataError = AmbiguousDataError Text deriving (Show, Eq, Read, Typeable)


instance Exception HTMLParsingError
instance Exception DataMissingError
instance Exception DataFormatError
instance Exception DomainCrossedError
instance Exception DuplicateFileError
instance Exception AmbiguousDataError

-- |Construct a 'DataMissingError' with the error message
--  @Expected element X not found!@".
dataMissingError :: URL -> Text -> DataMissingError
dataMissingError url el = DataMissingError url $ Just $ "Expected element '" `append` el `append` " ' not found!"

-- |Construct a 'DataMissingError', but don't specify the name of the element
--  that was missing.
dataMissingError' :: URL -> DataMissingError
dataMissingError' url = DataMissingError url Nothing

-- |Construct a 'DataFormatError'.
dataFormatError :: URL -> Text -> DataFormatError
dataFormatError url msg = DataFormatError url $ Just msg

-- |Construct a 'DataFormatError', with a default error message.
dataFormatError' :: URL -> DataFormatError
dataFormatError' url = DataFormatError url Nothing

-- |Construct a 'DuplicateFileError'.
duplicateFileError :: Text -> Text -> DuplicateFileError
duplicateFileError o n = DuplicateFileError (Just o) n

-- |Construct a 'DuplicateFileError', but don't specific the name of the file
--  that was attempted to be renamed.
duplicateFileError' :: Text -> DuplicateFileError
duplicateFileError' = DuplicateFileError Nothing

show' :: HttpException -> String
show' (StatusCodeException status headers _) =
   "Status code: " ++ show (Ty.statusCode status)
   ++ unpack (decodeUtf8 $ fromStrict $ Ty.statusMessage status)
show' (InvalidUrlException url err) =
   "Invalid URL '" ++ url ++ "'!\n" ++ err
show' (TooManyRedirects _) = "Too many redirects!"
show' (UnparseableRedirect _) = "Unparseable redirect!"
show' (TooManyRetries) = "Too many retries!"
show' (HttpParserException s) = "Couldn't parse HTTP!\nDetails: " ++ s
show' (HandshakeFailed) = "Handshake failed!"
show' (OverlongHeaders) = "Overlong headers!"
show' (ResponseTimeout) = "Response timeout!"
show' (FailedConnectionException host port) =
   "Failed connecting to " ++ host ++ ":" ++ show port ++ "!"
show' (FailedConnectionException2 host port _ _) =
   "Failed connecting to " ++ host ++ ":" ++ show port ++ "!"
show' (ExpectedBlankAfter100Continue) = "Expected blank after 100 Continue!"
show' (InvalidStatusLine l) =
   "Invalid status line '" ++ unpack (decodeUtf8 $ fromStrict l) ++ "'!"
show' (InvalidHeader l) =
   "Invalid header '" ++ unpack (decodeUtf8 $ fromStrict l) ++ "'!"
show' (InternalIOException _) = "Internal IO exception!"
show' (ProxyConnectException _ _ _) = "Proxy connection exception!"
show' (NoResponseDataReceived) = "Empty response!"
show' (TlsException e) = "TLS exception: " ++ (show e)
show' (TlsNotSupported) = "TLS not supported!"
show' (ResponseBodyTooShort e a) = "Response body too short. Expected " ++
   show e ++ ", got " ++ show a ++ "!"
show' (InvalidChunkHeaders) = "Invalid chunk headers!"
show' (IncompleteHeaders) = "Incomplete headers!"
show' (InvalidDestinationHost host) =
   "Invalid destination host '" ++ unpack (decodeUtf8 $ fromStrict host) ++ "'!"
show' (HttpZlibException _) = "Zlib exception!"

