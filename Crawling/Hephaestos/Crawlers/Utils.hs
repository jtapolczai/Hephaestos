{-# LANGUAGE OverloadedStrings #-}

-- |Helper functions for building crawlers.
module Crawling.Hephaestos.Crawlers.Utils where

import Prelude hiding ((++))

import Control.Arrow
import Control.Exception
import Data.Char
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.List.Split
import Data.Maybe (mapMaybe)
import Data.ListLike (ListLike(append), StringLike(fromString))
import qualified Data.Text.Lazy as T
import Data.Void
import qualified Network.URI as N
import Numeric.Peano

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.XPath

import Debug.Trace

-- |Tries to turn a text into an absolute link.
--  If a Text can parsed, then one two things will be done:
--
--  * if it is a relative link, it will be combined with the given URI into an
--    absolute one.
--  * if it is absolute, it is left as-is.
--
--  If a Text cannot be parsed, a Failure result with a 'DataFormatError'
--  will be created.
makeLink :: N.URI -- ^URI of the current page (for making relative links absolute).
          -> (N.URI -> FetchResult SomeException) -- ^The FetchResult into which the links should be wrapped. Commonly 'Inner' or 'Blob'.
          -> T.Text
          -> (FetchResult SomeException)
makeLink uri f u = maybe (Failure (dataFormatError (fromString $ show uri) errMsg) Nothing)
                         (f . flip N.nonStrictRelativeTo uri)
                         (N.parseURIReference $ T.unpack u)
   where
      errMsg = "Couldn't parse '" `append` u `append` "' as URI!"
