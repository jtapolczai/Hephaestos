{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |The common types used by the other modules.
--
--  This module also includes a hierarchy of errors types, organized
--  as follows (names in curly brackets indicate existential types/classes,
--  ones without indicate regular ones):
--
--  * DuplicateFileError
--  * {SomeDataError}
--    * DomainCrossedError
--    * {SomeMissingError}
--      * DataMissingError
--    * {SomeAmbiguityError}
--      * AmbiguousDataError
--      * AmbiguousKeyError
--    * {SomeFormatError}
--      * DataFormatError
--      * {SomeParsingError}
--        * HTMLParsingError
--        * MetadataParsingError
module Crawling.Hephaestos.Fetch.Types (
   module X,
   URL,
   WildcardURL,
   HTTPStatus,
   -- * Exceptions
   DuplicateFileError(..),
   SomeDataError(..),
   SomeMissingError(..),
   DataMissingError(..),
   DomainCrossedError(..),
   SomeAmbiguityError(..),
   AmbiguousDataError(..),
   AmbiguousKeyError(..),
   SomeFormatError(..),
   DataFormatError(..),
   SomeParsingError(..),
   HTMLParsingError(..),
   MetadataParsingError(..),
   URIParsingError(..),
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

-- |A file with the given name already existed.
data DuplicateFileError = DuplicateFileError (Maybe Text) Text deriving (Show, Eq, Typeable)
instance Exception DuplicateFileError

-- |The class of all data errors (missing data, wrong format, etc.).
data SomeDataError = forall e.Exception e => SomeDataError e deriving (Typeable)
instance Show SomeDataError where show (SomeDataError e) = show e
instance Exception SomeDataError

dataErrorUpcast :: (Exception a) => a -> SomeException
dataErrorUpcast = toException . SomeDataError
dataErrorDowncast :: (Exception a) => SomeException -> Maybe a
dataErrorDowncast x = do {SomeDataError y <- fromException x; cast y}

data SomeMissingError = forall e.Exception e => SomeMissingError e deriving (Typeable)
instance Show SomeMissingError where show (SomeMissingError e) = show e
instance Exception SomeMissingError where
   toException = dataErrorUpcast
   fromException = dataErrorDowncast

missingErrorUpcast :: (Exception a) => a -> SomeException
missingErrorUpcast = toException . SomeMissingError
missingErrorDowncast :: (Exception a) => SomeException -> Maybe a
missingErrorDowncast x = do {SomeMissingError y <- fromException x; cast y}

-- |A piece of expected data was missing on a page.
data DataMissingError = DataMissingError URL (Maybe Text) deriving (Show, Eq, Typeable)
instance Exception DataMissingError where
   toException = missingErrorUpcast
   fromException = missingErrorDowncast

-- |A URL lay outside of an expected domain.
data DomainCrossedError = DomainCrossedError WildcardURL URL deriving (Show, Eq, Typeable)
instance Exception DomainCrossedError where
   toException = dataErrorUpcast
   fromException = dataErrorDowncast

-- |The class of all ambiguity errors.
data SomeAmbiguityError = forall e. Exception e => SomeAmbiguityError e deriving (Typeable)
instance Show SomeAmbiguityError where show (SomeAmbiguityError e) = show e
instance Exception SomeAmbiguityError where
   toException = dataErrorUpcast
   fromException = dataErrorDowncast

ambiguityErrorUpcast :: (Exception a) => a -> SomeException
ambiguityErrorUpcast = toException . SomeAmbiguityError
ambiguityErrorDowncast :: (Exception a) => SomeException -> Maybe a
ambiguityErrorDowncast x = do {SomeAmbiguityError y <- fromException x; cast y}

-- |More than one piece of suitable data was found.
data AmbiguousDataError = AmbiguousDataError deriving (Show, Eq, Typeable)
instance Exception AmbiguousDataError where
   toException = ambiguityErrorUpcast
   fromException = ambiguityErrorDowncast

-- |More than one value was found for a specific key.
data AmbiguousKeyError = AmbiguousKeyError deriving (Show, Eq, Typeable)
instance Exception AmbiguousKeyError where
   toException = ambiguityErrorUpcast
   fromException = ambiguityErrorDowncast

-- |The class of all data format errors.
data SomeFormatError = forall e.Exception e => SomeFormatError e deriving (Typeable)
instance Show SomeFormatError where show (SomeFormatError e) = show e
instance Exception SomeFormatError where
   toException = dataErrorUpcast
   fromException = dataErrorDowncast

formatErrorUpcast :: (Exception a) => a -> SomeException
formatErrorUpcast = toException . SomeFormatError
formatErrorDowncast :: (Exception a) => SomeException -> Maybe a
formatErrorDowncast x = do {SomeFormatError y <- fromException x; cast y}

-- |A general data format error.
data DataFormatError = DataFormatError URL (Maybe Text) deriving (Show, Eq, Typeable)
instance Exception DataFormatError where
   toException = formatErrorUpcast
   fromException = formatErrorDowncast

-- |The class of all data format errors.
data SomeParsingError = forall e.Exception e => SomeParsingError e deriving (Typeable)
instance Show SomeParsingError where show (SomeParsingError e) = show e
instance Exception SomeParsingError where
   toException = formatErrorUpcast
   fromException = formatErrorDowncast

parsingErrorUpcast :: (Exception a) => a -> SomeException
parsingErrorUpcast = toException . SomeParsingError
parsingErrorDowncast :: (Exception a) => SomeException -> Maybe a
parsingErrorDowncast x = do {SomeParsingError y <- fromException x; cast y}

-- |The content of a page could not be parsed as HTML.
data HTMLParsingError = HTMLParsingError URL deriving (Show, Eq, Typeable)
instance Exception HTMLParsingError where
   toException = parsingErrorUpcast
   fromException = parsingErrorDowncast

-- |The content of a page could not be parsed as HTML.
data MetadataParsingError = MetadataParsingError Text deriving (Show, Eq, Typeable)
instance Exception MetadataParsingError where
   toException = parsingErrorUpcast
   fromException = parsingErrorDowncast

-- |The content of a page could not be parsed as HTML.
data URIParsingError = URIParsingError URL deriving (Show, Eq, Typeable)
instance Exception URIParsingError where
   toException = parsingErrorUpcast
   fromException = parsingErrorDowncast




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
duplicateFileError o = DuplicateFileError (Just o)

-- |Construct a 'DuplicateFileError', but don't specific the name of the file
--  that was attempted to be renamed.
duplicateFileError' :: Text -> DuplicateFileError
duplicateFileError' = DuplicateFileError Nothing
