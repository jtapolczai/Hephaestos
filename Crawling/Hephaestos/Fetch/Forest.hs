{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--  Errors do not stop the traversal; instead, all occurring errors are collected
downloadForest :: forall a resColl b.
                  (Injective a String,
                   Co.Foldable resColl (SuccessorNode SomeException b),
                   Co.Unfoldable resColl (SuccessorNode SomeException b),
                   Appendable [SuccessorNode SomeException b] resColl)
               => Manager
               -> Successor SomeException b
               -> (Request -> Request)
               -> a
               -> resColl
               -> ErrorIO' resColl
downloadForest m succ reqMod saveLocation = Co.foldlM collect Co.empty
   where
      -- generic writing function for plain text/XML/binary/info data.
      --writeValue :: (a -> BL.ByteString) -> URL -> a -> ErrorIO (Maybe )
      --writeValue = undefined
      writeValue xs f url v ext =
         (do uuid <- liftIO nextRandom >$> showT
             saveURL saveLocation (url++uuid++ext) $ f v
             return xs)
         `catchError`
         (\e -> return $ flip Co.insert xs v)

      --formatInfo :: DynNode -> BL.ByteString
      formatInfo x = T.encodeUtf8 $ k ++ to "\n" ++ v
         where (k,v) = infoKey &&& infoValue $ nodeRes x

      collect :: resColl -> SuccessorNode SomeException b -> ErrorIO' resColl

      --re-run fetchTree. The most complex case.
      collect xs (SuccessorNode st (Failure e True) reqF url) =
         do res <- extractResults $ fetchTree m succ (reqF.reqMod) st url
            ys <- downloadForest m succ reqMod saveLocation res
            return $ ys `append` xs

      -- try to download the Blob and insert a failure node in case of error
      collect xs (SuccessorNode st Blob reqF url) = undefined
         {-(downloadSave m (reqF.reqMod) saveLocation url >> return xs)
         `catchError`
         (\e -> return $ flip Co.insert xs
                         (SuccessorNode st (Failure e False) reqF url))-}
      -- try to re-download (as Blob)
      collect xs (SuccessorNode st (Failure e False) reqF url) =
         collect xs (SuccessorNode st Blob reqF url)

      -- save plain text, XML, info, and binary data locally
      collect xs n@(SuccessorNode _ (PlainText _) _ url) =
         writeValue xs (T.encodeUtf8 . fromPlainText . nodeRes) url n $ to ".txt"
      collect xs n@(SuccessorNode st (XmlResult _) _ url) =
         writeValue xs (B.encode . fromBinary . nodeRes) url n $ to ".xml"
      collect xs n@(SuccessorNode _ (BinaryData _) _ url) =
         writeValue xs (fromBinary . nodeRes) url n $ to ".bin"
      collect xs n@(SuccessorNode _ (Info _ _) _ url) =
         writeValue xs formatInfo url n $ to ".info"

      -- otherwise-case: keep other kinds of nodes as-is.
      collect xs n = return $ Co.insert n xs

-- |The class of data structures to which can be appended.
class Appendable a b where
   append :: a -> b -> b

instance Ord a => Appendable [a] (Set a) where
   append xs ys = S.fromList xs `S.union` ys

instance Appendable [a] [a] where
   append = (L.++)

instance Co.Unfoldable [a] a where
   insert = (:)
   empty = []
   singleton = (:[])

instance Ord a => Co.Unfoldable (S.Set a) a where
   insert = S.insert
   empty = S.empty
   singleton = S.singleton

instance Ord a => Co.Map (S.Set a) a () where
   delete = S.delete
   member = S.member
   union = S.union
   intersection = S.intersection
   difference = S.difference
   isSubset = S.isSubsetOf
   isProperSubset = S.isProperSubsetOf
   lookup a s = if S.member a s then Just () else Nothing
   alter _ _ s = s
   mapWithKey _ s = s
   unionWith _ = S.union
   intersectionWith _ = S.intersection
   differenceWith _ = error "differenceWith undefined for sets"
   isSubmapBy = error "isSubmapBy undefined for sets"
   isProperSubmapBy = error "isProperSubmapBy undefined for sets"
