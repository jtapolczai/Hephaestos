{-# LANGUAGE GADTs #-}

-- |Contains printing of localized error messages.
module Crawling.Hephaestos.CLI.Errors where

import Prelude hiding (error)

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Functor.Monadic ((>$>))
import Data.Text.Lazy
import System.IO.Error
import System.REPL (putErrLn)

import Crawling.Hephaestos.CLI.Color (error)
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.I18N

-- |Returns a human-friendly, localized error text for a recognized exception.
--  If an exception is not recognized, Nothing is returned.
--
--  Since this function can deal with any exception (like "Control.Monad.Catch"'s
--  'catchAll'), it shouldn't be used as an error handler; it should only be
--  used to get the localized error message for a specific exception which
--  one has already caught.
errorMsg :: Exception e => Lang -> e -> IO Text
errorMsg l e = throwM e `catches` handlers
   where
      msg' = return . msg l

      -- prints one of two messages, depending on whether the IOException
      -- has an associated filename
      fnMsg :: IOException -> a -> (Text -> a) -> a
      fnMsg e err1 err2 = maybe err1 err2 (ioeGetFileName e >$> pack)

      handlers = [Handler duplicateFileH,
                  Handler domainCrossedH,
                  Handler dataMissingH,
                  Handler ambiguousDataH,
                  Handler ambiguousKeyH,
                  Handler dataFormatH,
                  Handler htmlParsingH,
                  Handler metadataParsingH,
                  Handler uriParsingH,
                  Handler ioH,
                  Handler showH]

      duplicateFileH :: DuplicateFileError -> IO Text
      duplicateFileH (DuplicateFileError Nothing t) = msg' $ MsgDuplicateFileErr1 t
      duplicateFileH (DuplicateFileError (Just s) t) = msg' $ MsgDuplicateFileErr s t

      domainCrossedH :: DomainCrossedError -> IO Text
      domainCrossedH (DomainCrossedError d u) = msg' $ MsgDomainCrossedErr d u

      dataMissingH :: DataMissingError -> IO Text
      dataMissingH (DataMissingError d Nothing) = msg' $ MsgDataMissingErr1 d
      dataMissingH (DataMissingError d (Just u)) = msg' $ MsgDataMissingErr d u

      ambiguousDataH :: AmbiguousDataError -> IO Text
      ambiguousDataH AmbiguousDataError = msg' $ MsgAmbiguousDataErr

      ambiguousKeyH :: AmbiguousKeyError -> IO Text
      ambiguousKeyH (AmbiguousKeyError k) = msg' $ MsgAmbiguousKeyErr k

      dataFormatH :: DataFormatError -> IO Text
      dataFormatH (DataFormatError d) = msg' $ MsgDataFormatErr d

      htmlParsingH :: HTMLParsingError -> IO Text
      htmlParsingH (HTMLParsingError d) = msg' $ MsgHTMLParsingErr d

      metadataParsingH :: MetadataParsingError -> IO Text
      metadataParsingH (MetadataParsingError d) = msg' $ MsgMetadataParsingErr d

      uriParsingH :: URIParsingError -> IO Text
      uriParsingH (URIParsingError d) = msg' $ MsgURIParsingErr d

      ioH :: IOException -> IO Text
      ioH x | isDoesNotExistError x = msg' $ fnMsg x MsgFileDoesNotExist MsgFileDoesNotExist1
            | isPermissionError x && ioeGetLocation x == "renameFile" =
               msg' $ fnMsg x MsgRenameNoPermission MsgRenameNoPermission1
            | isIllegalOperation x && ioeGetLocation x == "renameFile" =
               msg' $ fnMsg x MsgRenameIllegalOperation MsgRenameIllegalOperation1
            | isFullError x = msg' $ fnMsg x MsgFullError MsgFullError1
            | otherwise = msg' $ MsgSomeIOError $ show x

      showH :: SomeException -> IO (Text)
      showH x = msg' $ MsgOtherError $ show x

-- |Prints an error text with 'error' to stderr.
printError :: MonadIO m => Text -> m ()
printError = error . liftIO . putErrLn
