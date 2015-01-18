{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
module System.Directory.Generic where

import Prelude hiding (FilePath)

import Control.Arrow
import Control.Monad.Catch
import Data.Functor.Monadic
import Data.ListLike (StringLike(fromString))
import qualified Data.Text.Lazy as T
import Data.Types.Isomorphic
import qualified System.Directory as D
import Filesystem.Path.CurrentOS

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.ErrorHandling

import Debug.Trace

-- |Tries to rename a file, failing with an exception if the file exists.
rename :: FilePath -- ^Directory containing the file.
       -> FilePath -- ^Old filename.
       -> FilePath -- ^New filename.
       -> IO ()
rename dir old new =
   D.doesFileExist (encodeString new')
   >>= \case True -> throwM $ duplicateFileError (fromString $ encodeString old') (fromString $ encodeString new')
             False -> D.renameFile (encodeString old') (encodeString new')
   where
      old' = dir </> old
      new' = dir </> new

-- |Turns a lazy Text into a FilePath.
fromText' :: T.Text -> FilePath
fromText' = fromText . T.toStrict

-- |Turns a FilePath into a lazy Text.
--  As FilePath's 'toText' might only return an approximation of the actual
--  path, this function should only be used for human-readable error messages,
--  not IO operations.
toText' :: FilePath -> T.Text
toText' = either T.fromStrict T.fromStrict . toText
