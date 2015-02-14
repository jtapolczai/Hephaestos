{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

-- |General types used by other modules.
module Crawling.Hephaestos.Fetch.Types.General (
   URL,
   WildcardURL,
   HTTPStatus,
   Path,
   Collection,
   ) where

import qualified Data.Collections as Co
import qualified Data.Collections.BulkInsertable as Co
import Data.Text.Lazy

-- |A URL.
type URL = Text
-- |A URL with possible wildcards (*) in it.
type WildcardURL = Text

-- |A path in a tree.
type Path n = [n]

-- |A numerical HTTP response status.
type HTTPStatus = Int

-- |List- or setlike collections.
type Collection (c :: (* -> *)) (a :: *) =
   (Co.Foldable (c a) a,
    Co.Unfoldable (c a) a,
    Co.BulkInsertable [a] (c a))
