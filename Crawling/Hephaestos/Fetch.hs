{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Basic downloading and file saving functions.
module Crawling.Hephaestos.Fetch (
   -- * Downloading
   simpleDownload,
   download,
   saveFile,
   downloadsFolder,

   module Crawling.Hephaestos.Fetch.ErrorHandling,
   )where

import Prelude hiding (concat, reverse, takeWhile, (++), putStrLn, writeFile, FilePath)

import Control.Arrow
import Control.Exception
import Control.Lens (makeLenses, (&), (%~), (^.))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Foldl (FoldM(..), foldM)
import Data.Aeson
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Collections as Co
import Data.Char (toLower)
import Data.Dynamic
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.ListLike (ListLike(append))
import Data.ListLike.IO (ListLikeIO(writeFile))
import Data.Types.Isomorphic
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import Network.URI (URI)
import System.Directory
import Filesystem.Path.CurrentOS' hiding (append, encode)

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor
import Crawling.Hephaestos.Fetch.ErrorHandling
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
         -> URI -- ^The URL
         -> IO BL.ByteString
download man reqF url =
   do req' <- parseUrl $ show url
      let req = reqF req'
      res <- withSocketsDo $ httpLbs req man
      putStrLn (show url `append` " downloaded.")
      return $ responseBody res

-- |Saves the contents of a HTTP response to a local file.
--  If the save directory does not exist, it will be created.
--
--  This function doesn't catch any exceptions and doesn't wrap exceptions
--  into failure nodes.
--
--  If the result type is a failure and the 'maxFailureNodes' setting
--  is 0, the file won't be saved at all.
--
--  This function is partial; inner nodes will not be saved
--  (except when they are wrapped in failure-nodes).
saveFile :: Show e
         => FetchOptions
         -> FilePath -- ^The root of the filename under which to save. Should not contain the extension.
         -> FetchResult e i -- ^Contents of the file
         -> IO (Maybe FilePath) -- ^The actual filename under which the response was saved.
saveFile opts fn response
   | isFailure response && (opts ^. maxFailureNodes) <<= 0 = return Nothing
   | otherwise = do
      let path = opts ^. savePath
      createDirectoryIfMissing True (encodeString path)
      content <- action response
      writeFile (encodeString $ path </> fn <.> ext response) content
      return $ Just $ fn <.> ext response
   where
      (<<=) Nothing _ = False
      (<<=) (Just x) y = x <= y

      -- downloads a Blob and gets the contents
      action (Blob _ url reqMod) = download (opts ^. manager)
                                            (reqMod.(opts ^. reqFunc))
                                            url

      --gets a ByteString out of a non-Blob leaf
      action (PlainText _ p) = return $ T.encodeUtf8 p
      action (XmlResult _ p) = return $ B.encode p
      action (BinaryData _ p) = return $ p
      action (Info _ k v) = return $ encode $ object ["key" .= k, "value" .= v]
      action f@(Failure _ _) = return $ encode $ action' f (opts ^. maxFailureNodes) 0

      -- saves a chain of failure nodes, omitting some if the maxFailureNodes
      -- limit is reached.
      -- we have two "modes": the regular one, in which we create a nested
      -- object, and the "limit reached" one, in which we go straight to
      -- the root and count the number of omitted nodes along the way.
      action' :: Show e => FetchResult e i -> Maybe Int -> Int -> Value
      action' (Failure e (Just (orig, _))) (Just 0) omitted = action' orig (Just 0) $ omitted + 1
      action' f@(Failure e (Just (orig, name))) limit omitted =
         object' ["error" .= show e, "type" .= ext f,
                  "child" .= action' orig (limit >$> subtract 1) omitted]
                  [("filename", name >$> encodeString)]

      -- the root of a failure chain
      action' f _ n = object' ["type" .= ext f]
                              [("url", getURL f >$> show >$> toJSON),
                               ("omitted", n' >$> toJSON),
                               ("error", getError f >$> show >$> toJSON)]
         where
            n' = if n == 0 then Nothing else Just n

      -- creates a JSON object out of a list of mandatory and a list of
      -- optional fields
      object' xs = object . (xs L.++) . map (\(k,Just v) -> k.=v) . filter (isJust.snd)

-- |Gets the user's Downloads folder. This is assumed to be
--  the directory named \"Dowloads\" (case sensitive)
--  in the user's home directory.
downloadsFolder :: (MonadIO m, Functor m) => m FilePath
downloadsFolder = liftIO getHomeDirectory
                  >$> decodeString
                  >$> (</> decodeString "Downloads")
                  >$> encodeString
                  >>= liftIO . canonicalizePath
                  >$> decodeString


