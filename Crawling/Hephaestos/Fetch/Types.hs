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
   module General,
   -- * Exceptions
   module Errors,
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
import Control.Concurrent.STM.Utils
import Control.Lens.TH (makeLenses)
import Data.Default
import qualified Data.IntMap as IM
import Filesystem.Path.CurrentOS (FilePath)
import Network.HTTP.Conduit (Request, Manager)

import Crawling.Hephaestos.Fetch.Types.Errors as Errors
import Crawling.Hephaestos.Fetch.Types.General as General

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
