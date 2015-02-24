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
   TaskCat(..),
   FetchOptions(..),
   addReferer,
   manager,
   reqFunc,
   savePath,
   maxFailureNodes,
   threadPoolSize,
   saveFetchState,
   saveRequestModifier,
   downloadCategories,
   downloadStats,
   taskLimit,
   escapingFunction,
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
import Control.Exception (SomeException)
import Control.Lens.TH (makeLenses)
import Data.Default
import qualified Data.Map as M
import Filesystem.Path.CurrentOS' (FilePath, Escaping)
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
                     | Failed SomeException
                       -- ^The download failed because of network or local IO
                       --  issues.
   deriving (Show)

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
   } deriving (Show)

makeLenses ''Download

instance Default Download where
   def = Download 0 Nothing "" WaitingForResponse


-- |An identifier for a task category.
newtype TaskCat = TaskCat Int deriving (Eq, Ord)

-- |Configuration data for fetch processes.
--  This record represents global options that a complex fetching function
--  might take into account.
data FetchOptions = FetchOptions {
   _addReferer :: Bool,
   _manager :: Manager,
   _reqFunc :: Request -> Request,
   _savePath :: FilePath,
   _maxFailureNodes :: Maybe Int,
   _threadPoolSize :: Int,
   _saveFetchState :: Bool,
   _saveRequestModifier :: Bool,
   _downloadCategories :: TaskCategories TaskCat Download,
   _downloadStats :: TVar (M.Map TaskCat Int),
   _taskLimit :: TaskLimit,
   _escapingFunction :: Escaping}

makeLenses ''FetchOptions
