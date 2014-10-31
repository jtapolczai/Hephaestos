{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- |Basic downloading and file saving functions.
module Crawling.Hephaestos.Fetch (
   -- * Downloading
   simpleDownload,
   download,
   downloadSave,
   downloadSave',
   saveURL,
   downloadFiles,
   downloadFiles',
   downloadsFolder,

   module Crawling.Hephaestos.Fetch.Types,
   module Crawling.Hephaestos.Fetch.ErrorHandling,
   )where

import Prelude hiding (concat, reverse, takeWhile, (++), putStrLn)

import Control.Arrow
import Control.Exception
import Control.Monad.Except hiding (foldM)
import Control.Foldl (FoldM(..), foldM)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Collections as Co
import Data.Char (toLower)
import Data.Dynamic
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.List.Split (splitOn)
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

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Helper.String (stripParams, showT)
import System.REPL

-- |Gets the content of an URL.
--  @simpleDownload = withSocketsDo . simpleHttp@ and thus, caveats of
--  @simpleHttp@ apply.
simpleDownload :: URL -> IO BL.ByteString
simpleDownload = withSocketsDo . simpleHttp . T.unpack

-- |Downloads the contents of a URL and saves them to
--  a given location. If an error occurs, Left is returned.
--  The @Manager@-parameter is there to enable connection pooling.
download :: Manager -- ^Global connection manager.
         -> (Request -> Request) -- ^Modifiers to the request. 'id'
                                     --  leaves the request as-is.
         -> URL -- ^The URL
         -> ErrorIO BL.ByteString
download man reqF url =
   do req' <- catchHttp url $ parseUrl $ T.unpack url
      let req = reqF req'
      res <- catchHttp url $ withSocketsDo $ httpLbs req man
      putStrLn (url ++ (to " downloaded."))
      return $ responseBody res

-- |Saves the @ByteString@ (the contents of a response) to a local
--  file, under the filename given in the URL.
saveURL :: (Injective a String) => a -> URL -> BL.ByteString -> ErrorIO ()
saveURL savepathRoot url bs =
   do guardErr (null filename) (SomeException $ NetworkError url $ FormatError $ (to "URL '") ++ url ++ (to " doesn't contain a filename."))
      catchIO url FileError $ createDirectoryIfMissing True savePath
      catchIO url FileError $ BL.writeFile (to savePath </> to filename) bs
      return ()
   where
      savePath = L.foldl' (</>) (to savepathRoot) filepath
      (filepath, filename) = (L.reverse . tail &&& head)
                             $ L.reverse
                             $ splitOn "/"
                             $ stripHttp
                             $ to
                             $ stripParams url

      -- strips the "http://" prefix (case-insensitive) from URLs, if present
      stripHttp x = case L.stripPrefix "http://" (map toLower x) of
                       Nothing -> x
                       Just _ -> drop (length "http://") x

-- |Downloads the contants of a URL and saves them to a given location.
--  This is a convenience wrapper around @download@.
downloadSave' :: (Injective a String) => Manager -> a -> URL -> ErrorIO ()
downloadSave' m = downloadSave m id

-- |Downloads the contants of a URL and saves them to a given location.
--  This is a convenience wrapper around @download@.
downloadSave :: (Injective a String)
             => Manager -> (Request -> Request) -> a -> URL -> ErrorIO ()
downloadSave m reqF fp url = download m reqF url >>= saveURL fp url


-- |Downloads a series of files to given location.
--  In case of error during a download, the function will attempt
--  to continue with the next file.
--  At the end, the list of errors are returned, if there were any.
downloadFiles :: (Iso a String) => Manager -> a -> [(URL, Request -> Request)] -> ErrorIO ()
downloadFiles m p urls = do errs <- mapErr_ (\(u,r) -> downloadSave m r p u) urls
                            if null errs then return ()
                                         else throwError $ SomeException errs

-- |Simpler version of @downloadFiles@.
--  Downloads a series of files to a given location.
--  The given path is appended to the user's Downloads directory,
--  assumed to be '<home>/Downloads'.
--  At the end, the list of errors are returned, if there were any.
downloadFiles' :: forall a.(Iso a String) => Manager -> a -> [(URL, Request -> Request)] -> ErrorIO ()
downloadFiles' m p u = do (dl :: a) <- catchIO (to "File") FileError downloadsFolder
                          downloadFiles m ((dl </> p) :: a) u

-- |Gets the user's Downloads folder. This is assumed to be
--  the directory named \"Dowloads\" (case sensitive)
--  in the user's home directory.
downloadsFolder :: (MonadIO m, Functor m, Injective String a) => m a
downloadsFolder = liftIO getHomeDirectory
                  >$> (</> "Downloads")
                  >>= canonicalizePath
                  >$> to . normalise


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
--downloadForest :: (Injective a String, Co.Collection resColl DynNode)
--               => Manager
--               -> (Request -> Request)
--               -> a
--               -> resColl
--               -> ErrorIO resColl
downloadForest m reqMod saveLocation = foldM fold'
   where
      fold' = FoldM collect (return Co.empty) return

      --collect :: resColl -> DynNode -> ErrorIO resColl
      collect xs (SuccessorNode st (Failure e False) reqF url) =
         undefined --try to download
      collect xs (SuccessorNode st (Failure e True) reqF url) =
         undefined -- re-run fetchTree
      -- try to download the Blob and insert a failure node in case of error
      collect xs (SuccessorNode st Blob reqF url) =
         (downloadSave m (reqF.reqMod) saveLocation url >> return xs)
         `catchError`
         (\e -> return $ flip Co.insert xs
                         (SuccessorNode st (Failure e False) reqF url))
      collect xs (SuccessorNode st (PlainText t) reqF url) =
         (do uuid <- liftIO nextRandom >$> showT
             saveURL saveLocation (url++uuid) $ T.encodeUtf8 t)
         `catchError`
         (\e -> return $ flip Co.insert xs
                         (SuccessorNode st (PlainText t) reqF url))
      collect xs (SuccessorNode st (XmlResult t) reqF url) =
         undefined --save
      collect xs (SuccessorNode st (BinaryData t) reqF url) =
         undefined --save
      collect xs (SuccessorNode st (Info k v) reqF url) =
         undefined --save
