{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

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
module Crawling.Hephaestos.Fetch.Types (
   module X,
   URL,
   WildcardURL,
   HTTPStatus,
   Path,
   Collection,
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
   -- * Configuration data
   FetchOptions(..),
   addReferer,
   manager,
   reqFunc,
   savePath,
   maxFailureNodes,
   threadPoolSize,
   saveFetchState,
   saveRequestModifier,
   downloadSlots,
   taskLimit,
   DownloadStatus(..),
   Download(..),
   downloadBytes,
   downloadSize,
   downloadURL,
   downloadStatus,
   ) where

import Prelude hiding (FilePath)

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.Utils
import Control.Exception
import Control.Lens.TH (makeLenses)
import Control.Monad.Except
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Collections as Co
import qualified Data.Collections.BulkInsertable as Co
import qualified Data.Collections.Instances as Co
import Data.Default
import qualified Data.IntMap as IM
import Data.Text.Lazy hiding (fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable
import Filesystem.Path.CurrentOS (FilePath)
import Network.HTTP.Conduit as X (Request, Manager, HttpException(..))
import qualified Network.HTTP.Types as Ty
import Text.XML.HXT.DOM.TypeDefs

-- |A URL.
type URL = Text
-- |A URL with possible wildcards (*) in it.
type WildcardURL = Text

-- |A path in a tree.
type Path n = [n]

-- |A numerical HTTP response status.
type HTTPStatus = Int

-- |List- or setlike collections.
type Collection (c :: (* -> *)) (a :: *) =
   (Co.Foldable (c a) a,
    Co.Unfoldable (c a) a,
    Co.BulkInsertable [a] (c a))

-- |Current status of a download task.
data DownloadStatus = WaitingForResponse
                      -- ^A request was sent, but no response has arrived yet.
                     | InProgress
                       -- ^The download is in progress.
                     | Finished
                       -- ^The download has finished successfully.
                     | FailedNetwork
                       -- ^The download failed because of network issues.
                     | FailedHTTP Int
                       -- ^The download failed the server replied with a non
                       --  2xx status code.
   deriving (Show, Eq, Read, Ord)

-- |Represents a (currently running) download task.
data Download = Download{
      _downloadBytes :: Integer,
      -- ^The number of bytes that have already been downloaded.
      _downloadSize :: Maybe Integer,
      -- ^The size of the resource (as given by the Content-Length HTTP
      --  header). This may not be available and, if it is, it should not be
      --  taken as authoritative. If this data is displayed at all, it should
      --  only be to give the user a rough estimate.
      _downloadURL :: URL,
      -- ^The URL from which the resource is being downloaded.
      _downloadStatus :: DownloadStatus
      -- ^Indicates whether the download is finished.
   } deriving (Show, Eq, Read, Ord)

makeLenses ''Download

instance Default Download where
   def = Download 0 Nothing "" WaitingForResponse

-- |Configuration data for fetch processes.
--  This record represents global options that a complex fetching function
--  might take into account.
data FetchOptions = FetchOptions {_addReferer :: Bool,
                                  _manager :: Manager,
                                  _reqFunc :: Request -> Request,
                                  _savePath :: FilePath,
                                  _maxFailureNodes :: Maybe Int,
                                  _threadPoolSize :: Int,
                                  _saveFetchState :: Bool,
                                  _saveRequestModifier :: Bool,
                                  _downloadSlots :: TVar (IM.IntMap Download),
                                  _taskLimit :: TaskLimit}

makeLenses ''FetchOptions

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

-- |Construct a 'DuplicateFileError', but don't specific the name of the file
--  that was attempted to be renamed.
duplicateFileError' :: Text -> DuplicateFileError
duplicateFileError' = DuplicateFileError Nothing
