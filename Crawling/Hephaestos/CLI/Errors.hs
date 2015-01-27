{-# LANGUAGE GADTs #-}

-- |Contains printing of localized error messages.
module Crawling.Hephaestos.CLI.Errors where

import Prelude hiding (error)

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Text.Lazy
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

      handlers = [Handler duplicateFileH,
                  Handler showH]

      duplicateFileH :: DuplicateFileError -> IO (Text)
      duplicateFileH (DuplicateFileError Nothing t) = msg' $ MsgDuplicateFileErr1 t
      duplicateFileH (DuplicateFileError (Just s) t) = msg' $ MsgDuplicateFileErr s t

      showH :: SomeException -> IO (Text)
      showH = return . pack . show

-- |Prints an error text with 'error' to stderr.
printError :: MonadIO m => Text -> m ()
printError = error . liftIO . putErrLn
