{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

-- |The common types used by the other modules.
module Crawling.Hephaestos.Fetch.Types (
   module X,
   URL,
   WildcardURL,
   HTTPStatus,
--   TextExtractor,
--   InfoExtractor,
--   Info,
--   NetworkError (..),
--   NetworkErrorKind (..),
--   dataFindingError,

   HTMLParsingError,
   DataMissingError,
   DomainCrossedError,
   dataMissingError,
   dataMissingError',
   htmlParsingError,
   domainCrossedError,

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
--type TextExtractor = XmlTree -> Maybe Text
-- |A function tries to extract a key-value-pair from a DOM tree.
--  The first value of the result is the key, the second is the
--  value and may be absent.
--type InfoExtractor = XmlTree -> Info Text Text


instance Exception e => Exception [e]

-- |The content of a page could not be parsed as HTML.
data HTMLParsingError = HTMLParsingError URL deriving (Show, Eq, Read, Typeable)
-- |A piece of expected data was missing on a page.
--
data DataMissingError = DataMissingError URL (Maybe Text) deriving (Show, Eq, Read, Typeable)
-- |A URL lay outside of an expected domain.
data DomainCrossedError = DomainCrossedError WildcardURL URL deriving (Show, Eq, Read, Typeable)

-- |Construct a 'DataMissingError'.
dataMissingError :: URL -> Text -> SomeException
dataMissingError url el = SomeException $ DataMissingError url $ Just $ "Expected element '" `append` el `append` " ' not found!"

-- |Construct a 'DataMissingError', but don't specify the name of the element
--  that was missing.
dataMissingError' :: URL -> SomeException
dataMissingError' url = SomeException $ DataMissingError url Nothing

-- |Construct a 'HTMLParsingError'.
htmlParsingError :: URL -> SomeException
htmlParsingError = SomeException . HTMLParsingError

-- |Construct a 'DomainCrossedError'.
domainCrossedError :: WildcardURL -> URL -> SomeException
domainCrossedError dom url = SomeException $ DomainCrossedError dom url

instance Exception HTMLParsingError
instance Exception DataMissingError
instance Exception DomainCrossedError

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
