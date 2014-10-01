{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |Basic downloading and file saving functions.
module Crawling.Hephaestos.Fetch (
   -- * Downloading
   withManager,
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

import Prelude hiding (concat, reverse, takeWhile, (++))

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Crawling.Hephaestos.Helper.String ((++))
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import System.Directory
import qualified System.FilePath as Px
import System.FilePath.Generic ((</>), normalise, FilePathT)

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Helper.Functor
import System.REPL

-- |Gets the content of an URL.
--  @simpleDownload = withSocketsDo . simpleHttp@ and thus, caveats of
--  @simpleHttp@ apply.
simpleDownload :: URL -> IO BL.ByteString
simpleDownload = withSocketsDo . simpleHttp . T.unpack

-- |Augments a function which takes a @Manager@ to one that
--  optionally takes one and returns it too.
--  If no manager is supplied, a new one is created and returned.
withManager :: MonadIO m
            => (Manager -> a -> m b) -- ^Function requiring a manager
            -> Maybe Manager -> a -> m b -- ^Lifted function.
withManager f m x =
   do m' <- case m of Nothing  -> liftIO $ newManager defaultManagerSettings
                      Just m'' -> return m''
      f m' x

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
      liftIO $ putStrLnT (url ++ " downloaded.")
      return $ responseBody res

-- |Saves the @ByteString@ (the contents of a response) to a local
--  file, under the filename given in the URL.
saveURL :: FilePathT -> URL -> BL.ByteString -> ErrorIO ()
saveURL savePath url bs =
   do let filename = T.reverse $ T.takeWhile (not . ('/'==)) $ T.reverse url
      guardErr (T.null filename) [NetworkError url $ FormatError $ "URL '" ++ url ++ " doesn't contain a filename."]
      catchIO url FileError $ createDirectoryIfMissing True (T.unpack savePath)
      catchIO url FileError $ BL.writeFile (T.unpack $ savePath </> filename) bs
      return ()

-- |Downloads the contants of a URL and saves them to a given location.
--  This is a convenience wrapper around @download@.
downloadSave' :: Manager -> FilePathT -> URL -> ErrorIO ()
downloadSave' m = downloadSave m id

-- |Downloads the contants of a URL and saves them to a given location.
--  This is a convenience wrapper around @download@.
downloadSave :: Manager -> (Request -> Request) -> FilePathT -> URL -> ErrorIO ()
downloadSave m reqF fp url = download m reqF url >>= saveURL fp url


-- |Downloads a series of files to given location.
--  In case of error during a download, the function will attempt
--  to continue with the next file.
--  At the end, the list of errors are returned, if there were any.
downloadFiles :: Manager -> FilePathT -> [(URL, Request -> Request)] -> ErrorIO ()
downloadFiles m p urls = do errs <- mapErr_ (\(u,r) -> downloadSave m r p u) urls
                            if null errs then return ()
                                         else throwError errs

-- |Simpler version of @downloadFiles@.
--  Downloads a series of files to a given location.
--  The given path is appended to the user's Downloads directory,
--  assumed to be '<home>/Downloads'.
--  At the end, the list of errors are returned, if there were any.
downloadFiles' :: Manager -> FilePathT -> [(URL, Request -> Request)] -> ErrorIO ()
downloadFiles' m p u = do dl <- catchIO "File" FileError downloadsFolder
                          downloadFiles m (dl </> p) u

-- |Gets the user's Downloads folder. This is assumed to be
--  the directory named \"Dowloads\" (case sensitive)
--  in the user's home directory.
downloadsFolder :: IO FilePathT
downloadsFolder = getHomeDirectory
                  >$> (Px.</> "Downloads")
                  >>= canonicalizePath
                  >$> normalise . T.pack
