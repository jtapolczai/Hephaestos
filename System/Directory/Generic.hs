{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module System.Directory.Generic where

import Prelude hiding (FilePath)

import Control.Arrow
import Control.Monad.Except
import Control.Monad.Trans
import Data.Either.Combinators (fromRight')
import Data.Functor.Monadic
import qualified Data.Text.Lazy as T
import Data.Types.Isomorphic
import qualified System.Directory as D
import Filesystem.Path.CurrentOS

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.ErrorHandling

import Debug.Trace

--createDirectoryIfMissing :: (MonadIO m, Injective a String) => Bool -> a -> m ()
--createDirectoryIfMissing b = liftIO . D.createDirectoryIfMissing b . to

getHomeDirectory :: (MonadIO m, Functor m, Injective String a) => m a
getHomeDirectory = to <$< liftIO D.getHomeDirectory

canonicalizePath :: (MonadIO m, Functor m, Iso a String)
                 => a -> m a
canonicalizePath = to <$=< (liftIO . D.canonicalizePath . to)

doesFileExist :: (MonadIO m, Functor m, Injective a String) => a -> m Bool
doesFileExist = liftIO . D.doesFileExist . to

renameFile :: (MonadIO m, Functor m, Injective a String) => a -> a -> m ()
renameFile old new = liftIO $ D.renameFile (to old) (to new)

-- |Tries to rename a file, failing with an exception if the file exists.
rename :: FilePath -- ^Directory containing the file.
       -> FilePath -- ^Old filename.
       -> FilePath -- ^New filename.
       -> ErrorIO ()
rename dir old new = case toText $ dir </> new of
   Left err -> addNetworkError (toText' old) $ FileError "Could not rename file!"
   Right new' -> doesFileExist' (to new')
                 >>= \case True -> duplicateFileError
                           False -> renameFile' (toText' $ dir </> old)
                                                (to new')
   where
      doesFileExist' f = catchIO f FileError (doesFileExist f)
      duplicateFileError = addNetworkError (toText' old) (FileError "File already exists!")
      renameFile' o n = catchIO o FileError (renameFile o n)

-- |ErrorIO-wrapper around 'System.Directory.createDirectoryIfMissing'.
createDirectoryIfMissing :: Bool -> FilePath -> ErrorIO ()
createDirectoryIfMissing t d = case toText d of
   Left _ -> addNetworkError (toText' d) $ FileError "Invalid directory path!"
   Right d' -> catchIO (to d') FileError $ D.createDirectoryIfMissing t (to d')

-- |Converts a 'FilePath' to lazy text, ignoring decoding errors.
--  Use only for paths already known to be valid.
toText' = to . fromEither . toText
   where
      fromEither (Left x) = x
      fromEither (Right x) = x
