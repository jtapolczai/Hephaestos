{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fetch (
   -- * Downloading
   simpleDownload,
   download,
   downloadSave,
   saveURL,
   downloadFiles,
   downloadFiles',

   module Fetch.Types,
   module Fetch.ErrorHandling,
   )where

import Prelude hiding (concat)

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import System.Directory
import System.FilePath

import Fetch.Types
import Fetch.ErrorHandling

-- |Gets the content of an URL.
--  @simpleDownload = withSocketsDo . simpleHttp@ and thus, caveats of
--  @simpleHttp@ apply.
simpleDownload :: URL -> IO BL.ByteString
simpleDownload = withSocketsDo . simpleHttp

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
download :: Manager -> URL -> ErrorIO BL.ByteString
download man url =
   do req' <- liftIO $ parseUrl url
      let req = req'{method = "GET"}
      res <- catchIO ConnectionError $ withSocketsDo $ httpLbs req man
      liftIO $ putStrLn (url ++ " downloaded.")
      return $ responseBody res

-- |Saves the @ByteString@ (the contents of a response) to a local
--  file, under the filename given in the URL.
saveURL :: FilePath -> URL -> BL.ByteString -> ErrorIO ()
saveURL savePath url bs =
   do let filename = reverse $ takeWhile (not . ('/'==)) $ reverse url
      guardErr (null filename) [FormatError $ "URL '" ++ url ++ " doesn't contain a filename."]
      catchIO FileError $ createDirectoryIfMissing True savePath
      catchIO FileError $ BL.writeFile (savePath </> filename) bs
      return ()

-- |Downloads the contants of a URL and saves them to a given location.
--  This is a convenience wrapper around @download@.
downloadSave :: Manager -> FilePath -> URL -> ErrorIO ()
downloadSave m fp url = download m url >>= saveURL fp url


-- |Downloads a series of files to given location.
--  In case of error during a download, the function will attempt
--  to continue with the next file.
--  At the end, the list of errors are returned, if there were any.
downloadFiles :: Manager -> FilePath -> [URL] -> ErrorIO [NetworkError]
downloadFiles m p = mapErr_ (\u -> downloadSave m p u `reportError` print')
   where print' = liftIO . print

-- |Simpler version of @downloadFiles@.
--  Downloads a series of files to a given location.
--  The given path is appended to the user's Downloads directory,
--  assumed to be '<home>/Downloads'.
--  At the end, the list of errors are returned, if there were any.
downloadFiles' :: Manager -> FilePath -> [URL] -> ErrorIO [NetworkError]
downloadFiles' m p u = do dl <- catchIO FileError dlFolder
                          downloadFiles m (dl</>p) u

-- |Gets the user's Downloads folder. This is assumed to be
--  the directory named \"Dowloads\" (case sensitive)
--  in the user's home directory.
dlFolder :: IO FilePath
dlFolder = liftM (</> "Downloads") getHomeDirectory
