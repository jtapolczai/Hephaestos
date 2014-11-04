{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

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
import Data.Void
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

--type DynNode = SuccessorNode SomeException Dynamic

-- |List- or setlike collections.
type Collection (c :: (* -> *)) (a :: *) =
   (Co.Foldable (c a) a,
    Co.Unfoldable (c a) a,
    Co.BulkInsertable [a] (c a))

-- |Maplike collections.
type Maplike (c :: (* -> * -> *)) (k :: *) (a :: *) =
   (Co.Unfoldable (c k a) (k,a),
    Co.Map (c k a) k a)


-- |A wrapper around 'downloadForest' that runs a crawler based on a
--  successor function, an initial state, and a URL.
complexDownload :: (Injective a String,
                    Collection results (SuccessorNode SomeException b))
                => Manager
                -> (Request -> Request) -- ^The global request modifier.
                -> a -- ^The root of the save path.
                -> Successor SomeException b -- ^Successor function.
                -> b -- ^Initial state for the successor function.
                -> URL -- ^Initial URL.
                -> ErrorIO' (results (SuccessorNode SomeException b))
complexDownload m reqF savePath succ initialState url =
   downloadForest m reqF savePath succ $ Co.singleton node
   where
      node = (SuccessorNode initialState Inner id url)

-- |Variant of 'complexDownload' that runs a crawler without a state.
complexDownload' :: (Injective a String,
                     Collection results (SuccessorNode SomeException Void))
                 => Manager
                 -> (Request -> Request) -- ^The global request modifier.
                 -> a -- ^The root of the save path.
                 -> Successor SomeException Void -- ^Successor function.
                 -> URL -- ^Initial URL.
                 -> ErrorIO' (results (SuccessorNode SomeException Void))
complexDownload' m reqF savePath succ url =
   complexDownload m reqF savePath succ undefined url

-- |Takes a collection of 'Successor' nodes and tries to download & save
--  them to disk. A successfully downloaded node is removed from the input set.
--
--  == Re-tryable failure nodes
--  A 'Failure' is called re-tryable if one of the following conditions is met:
--  * It has an original node that isn't a 'Failure';
--  * It has an original node that is a failure, but that node is re-tryable.
--  i.e. there must be a @(Just x)@ with @x@ being 'Blob', 'Inner', 'PlainText',
--  etc. at the end of the whole chain. That @x@ is called the __root__ of the
--  'Failure' nodes in the chain.
--
--  == Node handling
--  Nodes are handled in the following way:
--  * The Re-tryable failure nodes are re-tried with their root.
--  * Non-re-tryable failure nodes are kept as-is.
--  * Nodes that only require local IO action ('PlainText', 'BinaryData',
--    'XmlResult') are saved as local text/binary/xml files.
--  * 'Blob's are downloaded.
--  * For 'Inner' nodes, we re-run 'fetchTree'.
--
--  Both local and remote IO errors wrap the corresponding node into
--  a 'Failure' and replace it in the node set.
--
--  == Result set
--  The output set will only contain 'Failure' nodes (or be empty if there
--  were no failures) and those which aren' of the type mentioned above.
--  The failures can be used in another invocation of downloadForest.
--  If a node failed multiple times in a row, it will contain that history.
downloadForest :: forall a results b errors.
                  (Injective a String,
                   Collection results (SuccessorNode SomeException b))
               => Manager
               -> (Request -> Request) -- ^The global request modifier.
               -> a -- ^The root of the save path.
               -> Successor SomeException b -- ^Successor function.
               -> results (SuccessorNode SomeException b)
               -> ErrorIO' (results (SuccessorNode SomeException b))
downloadForest m reqMod saveLocation succ = Co.foldlM collect Co.empty
   where
      -- generic writing function for plain text/XML/binary/info data.
      writeValue xs f url v ext =
         (do uuid <- catchIO (to "Couldn't save data from '" ++ url ++ to "'!")
                             FileError nextRandom
                     >$> showT
             saveURL saveLocation (url++ to "_" ++uuid++ to ext) $ f v
             return xs)
         `catchError`
         (return . flip Co.insert xs. wrapFailure v)

      -- Wraps a node into a failure, given an exception.
      wrapFailure :: SuccessorNode SomeException b
                  -> SomeException
                  -> SuccessorNode SomeException b
      wrapFailure n@SuccessorNode{nodeRes=orig} e = n{nodeRes=Failure e (Just orig)}

      -- Complex download
      -------------------------------------------------------------------------

      --re-run fetchTree. The most complex case.
      collect xs (SuccessorNode st Inner reqF url) =
         do res <- extractResults $ fetchTree m succ (reqF.reqMod) st url
            ys <- downloadForest m reqMod saveLocation succ res
            return $ ys `Co.bulkInsert` xs

      -- Single download
      -------------------------------------------------------------------------

      -- try to download the Blob and insert a failure node in case of error
      collect xs n@(SuccessorNode st Blob reqF url) =
         (downloadSave m (reqF.reqMod) saveLocation url >> return xs)
         `catchError`
         (return . flip Co.insert xs . wrapFailure n)

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

      -- Re-run failures which have original nodes that show
      -- what was to be done originally.
      -------------------------------------------------------------------------
      collect xs s@SuccessorNode{nodeRes=Failure{originalNode=(Just orig)}} =
         collect xs s{nodeRes=orig}

      -- otherwise-case: keep other kinds of nodes as-is.
      -------------------------------------------------------------------------
      collect xs n = return $ Co.insert n xs
