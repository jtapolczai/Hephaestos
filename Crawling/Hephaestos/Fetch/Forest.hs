{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

-- |All-inclusive downloading. This module uses 'Crawling.Hephaestos.Fetch'
--  and 'Crawling.Hephaestos.Fetch.Tree' as building blocks to provide
--  all-in-one downloading, result extraction, saving, and retrying.
module Crawling.Hephaestos.Fetch.Forest where

import Prelude hiding ((++))

import Control.Arrow
import Control.Exception
import Control.Monad.Except hiding (foldM)
import Control.Foldl (FoldM(..), foldM)
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Collections as Co
import qualified Data.Collections.BulkInsertable as Co
import qualified Data.Collections.Instances as Co
import Data.Char (toLower)
import Data.Dynamic
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Tree.Monadic
import Data.UUID.V4
import Crawling.Hephaestos.Helper.String ((++))
import Data.Types.Isomorphic
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import System.Directory.Generic
import System.FilePath.Generic

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Helper.String (stripParams, showT)
import System.REPL

type DynNode = SuccessorNode SomeException Dynamic

complexDownload :: (Injective a String)
                => Manager
                -> Successor SomeException b
                -> (Request -> Request)
                -> a
                -> URL
                -> ErrorIO' (resColl, localErrs)
complexDownload = undefined

-- |List- or setlike collections.
type Collection (c :: (* -> *)) (a :: *) =
   (Co.Foldable (c a) a,
    Co.Unfoldable (c a) a,
    Co.BulkInsertable [a] (c a))

-- |Maplike collections.
type Maplike (c :: (* -> * -> *)) (k :: *) (a :: *) =
   (Co.Unfoldable (c k a) (k,a),
    Co.Map (c k a) k a)

-- |Takes a collection of 'Successor' nodes and tries to download & save
--  them to disk.
--
--  Nodes are handled in the following way:
--  * Failure nodes are re-tried (as either 'Inner' nodes or 'Blobs').
--    If an error occurs, the failure nodes are kept in the set.
--  * Nodes that only require local IO action ('PlainText', 'BinaryData',
--    'XmlResult') are saved as local text/binary/xml files. If an error
--    occurs during saving, the nodes are kept in the set.
--  * Blobs are downloaded. If an error occurs, a failure node is put
--    into the set.
--  * Inner nodes are not handled; they are kept as-is.
--
--  Errors do not stop the traversal; instead, they are collected and returned
--  as the second component of the result tuple.
--  The effect of an error on the nodes are as follows:
--  * If a node requires no remote IO ('PlainText', 'BinaryData', 'XmlResult',
--    'Info'), it is kept in the result set unchanged, so that a new save
--    attempt may be made at a later time.
--  * If a node does require remote IO ('Blob', 'Failure'), an appropriate
--    failure node is inserted.
--
--  The type signature might look daunting, but it works with most common
--  data types: @resColl@ can be a list or a set, and @localErrs@ is a map.
downloadForest :: forall a results b errors.
                  (Injective a String,
                   Collection results (SuccessorNode SomeException b),
                   Maplike errors (SuccessorNode SomeException b) SomeException)
               => Manager
               -> Successor SomeException b
               -> (Request -> Request)
               -> a
               -> results (SuccessorNode SomeException b)
               -> ErrorIO' (results (SuccessorNode SomeException b), errors (SuccessorNode SomeException b) SomeException)
downloadForest m succ reqMod saveLocation = Co.foldlM collect (Co.empty,Co.empty)
   where
      -- generic writing function for plain text/XML/binary/info data.
      {-writeValue :: (resColl, localErrs)
                 -> (SuccessorNode SomeException b -> BL.ByteString)
                 -> URL
                 -> SuccessorNode SomeException b
                 -> String
                 -> ErrorIO (resColl, localErrs)-}
      writeValue (xs,err) f url v ext =
         (do uuid <- catchIO (to "Couldn't save data from '" ++ url ++ to "'!")
                             FileError nextRandom
                     >$> showT
             saveURL saveLocation (url++ to "_" ++uuid++ to ext) $ f v
             return (xs,err))
         `catchError`
         (\e -> return (Co.insert v xs, Co.insertWith const v e err))

      --collect :: (resColl, localErrs) -> SuccessorNode SomeException b
      --                                -> ErrorIO' (resColl, localErrs)

      -- Complex download
      -------------------------------------------------------------------------

      --re-run fetchTree. The most complex case.
      collect (xs,err) (SuccessorNode st (Failure e True) reqF url) =
         do res <- extractResults $ fetchTree m succ (reqF.reqMod) st url
            (ys,err') <- downloadForest m succ reqMod saveLocation res
            return (ys `Co.bulkInsert` xs, err `Co.union` err')

      -- Single download
      -------------------------------------------------------------------------

      -- try to download the Blob and insert a failure node in case of error
      collect (xs,err) (SuccessorNode st Blob reqF url) = undefined
         (downloadSave m (reqF.reqMod) saveLocation url >> return xs)
         `catchError`
         (\e -> let n = (SuccessorNode st (Failure e False) reqF url) in
                return (Co.insert n xs, Co.insertWith const n e err))
      -- try to re-download (as Blob)
      collect xs s@SuccessorNode{nodeRes=Failure{}} = collect xs s{nodeRes=Blob}

      -- Local saving
      -------------------------------------------------------------------------

      -- save plain text, XML, info, and binary data locally
      collect xs n@(SuccessorNode _ (PlainText _) _ url) =
         writeValue xs (T.encodeUtf8 . fromPlainText . nodeRes) url n ".txt"
      collect xs n@(SuccessorNode st (XmlResult _) _ url) =
         writeValue xs (B.encode . fromBinary . nodeRes) url n ".xml"
      collect xs n@(SuccessorNode _ (BinaryData _) _ url) =
         writeValue xs (fromBinary . nodeRes) url n ".bin"
      collect xs n@(SuccessorNode _ (Info _ _) _ url) =
         writeValue xs formatInfo url n ".info"
         where
            formatInfo (SuccessorNode{nodeRes=Info k v}) =
               T.encodeUtf8 $ k ++ to "\n" ++ v

      -- otherwise-case: keep other kinds of nodes as-is.
      collect (xs,err) n = return $ (Co.insert n xs, err)
