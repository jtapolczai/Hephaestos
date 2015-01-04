{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- |The common types used by the other modules.
module Crawling.Hephaestos.Fetch.Types (
   module X,
   URL,
   WildcardURL,
   HTTPStatus,
   TextExtractor,
   InfoExtractor,
   Info,
   NetworkError (..),
   NetworkErrorKind (..),
   dataFindingError,
   ErrorIO,
   ErrorIO',
   -- * Configuration data
   FetchOptions(..),
   addReferer,
   manager,
   reqFunc,
   savePath,
   ) where

import Control.Exception
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Lazy hiding (fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable
import Network.HTTP.Conduit as X (Request, Manager, HttpException(..))
import qualified Network.HTTP.Types as Ty
import Text.XML.HXT.DOM.TypeDefs


-- |Configuration data for fetch processes.
--  This record represents global options that a complex fetching function
--  might take into account.
data FetchOptions = FetchOptions {_addReferer :: Bool,
                                  _manager :: Manager,
                                  _reqFunc :: Request -> Request,
                                  _savePath :: Text}

makeLenses ''FetchOptions

-- |A URL.
type URL = Text
-- |A URL with possible wildcards (*) in it.
type WildcardURL = Text

-- |A numerical HTTP response status.
type HTTPStatus = Int

-- |A function which tries to extract content from a DOM tree.
type TextExtractor = XmlTree -> Maybe Text
-- |A function tries to extract a key-value-pair from a DOM tree.
--  The first value of the result is the key, the second is the
--  value and may be absent.
type InfoExtractor = XmlTree -> Info Text Text

-- |Auxiliary information that was extracted from a page
--  but isn't the primary content.
type Info k v = (k, Maybe v)

-- |A network error, consisting of a 'NetworkErrorKind' and
--  a URL indicating the error's source (if any).
data NetworkError = NetworkError URL NetworkErrorKind
   deriving (Typeable)

instance Show NetworkError where
   show (NetworkError url kind) = unpack $
                                  "Error in '" `append` url `append` "': "
                                  `append` pack (show kind)

instance Exception NetworkError

instance Exception e => Exception [e]

-- |The sum type of all network or file errors
--  that occur during fetching URLs or saving files locally.
data NetworkErrorKind = HttpError HttpException -- ^A wrapped 'HttpException'.
                        | FileError Text -- ^A local IO error.
                        | FormatError Text -- ^A data formating error.
                        | DataFindingError Text -- ^Crucial data was not found in a data source.
                        | HTMLParsingError -- ^HTML content could not be parsed.

-- |Synonym for @NetworkError url DataFindingError "Expected element not found!"@
dataFindingError :: URL -> NetworkError
dataFindingError url = NetworkError url $ DataFindingError "Expected element not found!"

instance Show NetworkErrorKind where
   show (HttpError m) = "HTTP Error: " ++ show' m
   show (FileError m) = unpack m
   show (FormatError m) = unpack m
   show (DataFindingError m) = unpack m
   show (HTMLParsingError) = "Couldn't parse file as HTML!"

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

-- |An ExceptT-wrapper around IO. The type of all IO actions
--  in the program.
type ErrorIO a = ExceptT SomeException IO a
-- |The @* ->*@-kinded version of 'ErrorIO'. Useful
--  for when one wishes to use ErrorIO as the argument of a type
--  constructor.
type ErrorIO' = ExceptT SomeException IO
