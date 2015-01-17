{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- |Basic downloading and file saving functions.
module Crawling.Hephaestos.Fetch (
   -- * Downloading
   simpleDownload,
   download,
   saveURL,
   downloadsFolder,

   module Crawling.Hephaestos.Fetch.Types,
   module Crawling.Hephaestos.Fetch.ErrorHandling,
   )where

import Prelude hiding (concat, reverse, takeWhile, (++), putStrLn, writeFile, FilePath)

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
import qualified Data.Text.Lazy as T
import Data.ListLike (ListLike(append))
import Data.ListLike.IO (ListLikeIO(writeFile))
import Data.Types.Isomorphic
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import Network.URI (URI)
import System.Directory
import System.Directory.Generic
import Filesystem.Path.CurrentOS hiding (append)

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
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
         -> ErrorIO BL.ByteString
download man reqF url =
   do req' <- catchIO $ parseUrl $ show url
      let req = reqF req'
      res <- liftIO $ withSocketsDo $ httpLbs req man
      liftIO $ putStrLn (show url `append` " downloaded.")
      return $ responseBody res

-- |Saves the @ByteString@ (the contents of a response) to a local file.
saveURL :: (ListLikeIO s item)
        => FilePath -- ^The target folder.
        -> FilePath -- ^Filename to which to save.
        -> s -- ^Contents of the file.
        -> ErrorIO ()
saveURL savePath filename bs =
   do catchIO $ createDirectoryIfMissing True (encodeString savePath)
      catchIO $ writeFile (encodeString $ savePath </> filename) bs
      return ()

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


