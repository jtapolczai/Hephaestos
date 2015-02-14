{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Contains the following hierarchy of errors:
--
--  * DuplicateFileError
--  * {SomeDataError}
--    * DomainCrossedError
--    * {SomeMissingError}
--      * DataMissingError
--      * ReferencedFileMissingError
--    * {SomeAmbiguityError}
--      * AmbiguousDataError
--      * AmbiguousKeyError
--    * {SomeFormatError}
--      * DataFormatError
--      * URLHadNoNumberError
--      * {SomeParsingError}
--        * HTMLParsingError
--        * MetadataParsingError
module Crawling.Hephaestos.Fetch.Types.Errors (
   -- * Exceptions
   DuplicateFileError(..),
   SomeDataError(..),
   SomeMissingError(..),
   DataMissingError(..),
   ReferencedFileMissingError(..),
   DomainCrossedError(..),
   SomeAmbiguityError(..),
   AmbiguousDataError(..),
   AmbiguousKeyError(..),
   SomeFormatError(..),
   DataFormatError(..),
   URLHadNoNumberError(..),
   SomeParsingError(..),
   HTMLParsingError(..),
   MetadataParsingError(..),
   URIParsingError(..),
   dataMissingError,
   duplicateFileError,
   ) where

import Control.Exception
import Data.Text.Lazy
import Data.Typeable

import Crawling.Hephaestos.Fetch.Types.General

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

-- |A URL lay outside of an expected domain.
data DomainCrossedError = DomainCrossedError WildcardURL URL deriving (Show, Eq, Typeable)
instance Exception DomainCrossedError where
   toException = dataErrorUpcast
   fromException = dataErrorDowncast

-- |The class of "missing data" errors.
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

-- |A file referenced in a metadata file was missing,
data ReferencedFileMissingError = ReferencedFileMissingError Text deriving (Show, Eq, Typeable)
instance Exception ReferencedFileMissingError where
   toException = missingErrorUpcast
   fromException = missingErrorDowncast

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
data AmbiguousKeyError = AmbiguousKeyError Text deriving (Show, Eq, Typeable)
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
data DataFormatError = DataFormatError URL deriving (Show, Eq, Typeable)
instance Exception DataFormatError where
   toException = formatErrorUpcast
   fromException = formatErrorDowncast

-- |A URL didn't contain a string of digits, even thought a crawler required it.
data URLHadNoNumberError = URLHadNoNumberError URL deriving (Show, Eq, Typeable)
instance Exception URLHadNoNumberError where
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


-- |Construct a 'DataMissingError', but don't specify the name of the element
--  that was missing.
dataMissingError :: URL -> DataMissingError
dataMissingError url = DataMissingError url Nothing

-- |Construct a 'DuplicateFileError'.
duplicateFileError :: Text -> Text -> DuplicateFileError
duplicateFileError o = DuplicateFileError (Just o)
