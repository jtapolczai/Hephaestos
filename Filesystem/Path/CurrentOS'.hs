{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Alias for "Filesystem.Path.CurrentOS" which adds some helpers.
module Filesystem.Path.CurrentOS' (
   module CurrentOS,
   fromText',
   toText',
   (<.>),
   ) where

import Prelude hiding (FilePath)

import qualified Data.Text.Lazy as T
import Filesystem.Path.CurrentOS as CurrentOS hiding ((<.>))

-- |Turns a lazy Text into a FilePath.
fromText' :: T.Text -> FilePath
fromText' = fromText . T.toStrict

-- |Turns a FilePath into a lazy Text.
--  As FilePath's 'toText' might only return an approximation of the actual
--  path, this function should only be used for human-readable error messages,
--  not IO operations.
toText' :: FilePath -> T.Text
toText' = either T.fromStrict T.fromStrict . toText

-- |Synonym for addExtension
(<.>) :: FilePath -> T.Text -> FilePath
(<.>) x y = x `addExtension` T.toStrict y
