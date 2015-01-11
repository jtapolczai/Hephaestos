{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module System.Directory.Generic where

import Control.Arrow
import Control.Monad.Except
import Data.Functor.Monadic
import qualified Data.Text.Lazy as T
import Data.Types.Isomorphic
import qualified System.Directory as D
import System.FilePath.Generic

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.ErrorHandling

import Debug.Trace

createDirectoryIfMissing :: (MonadIO m, Injective a String) => Bool -> a -> m ()
createDirectoryIfMissing b = liftIO . D.createDirectoryIfMissing b . to

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
rename :: T.Text -- ^Directory containing the file.
       -> T.Text -- ^Old filename.
       -> T.Text -- ^New filename.
       -> ErrorIO ()
rename dir old new =
   catchIO (doesFileExist new')
   >>= \case True -> throwError $ duplicateFileError old' new'
             False -> catchIO $ renameFile old' new'
   where
      old' = dir </> old
      new' = dir </> new
