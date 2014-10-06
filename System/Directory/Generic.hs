{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
module System.Directory.Generic where

import Control.Arrow
import Control.Monad.Trans
import qualified Data.Text as T
import Data.Types.Isomorphic
import qualified System.Directory as D

import Crawling.Hephaestos.Helper.Functor

createDirectoryIfMissing :: (MonadIO m, Injective a String) => Bool -> a -> m ()
createDirectoryIfMissing b = liftIO . D.createDirectoryIfMissing b . to

getHomeDirectory :: (MonadIO m, Functor m, Injective String a) => m a
getHomeDirectory = to <$< liftIO D.getHomeDirectory

canonicalizePath :: (MonadIO m, Functor m, Iso a String)
                 => a -> m a
canonicalizePath = to <$=< (liftIO . D.canonicalizePath . to)

doesFileExist :: (MonadIO m, Functor m, Injective a String) => a -> m Bool
doesFileExist = liftIO . D.doesFileExist . to