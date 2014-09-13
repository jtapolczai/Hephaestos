{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Fetch (
   -- * Downloading,
   withManager,
   simpleDownload,
   download,
   downloadSave,
   saveURL,
   downloadFiles,
   downloadFiles',
   downloadsFolder,

   module Fetch.Types,
   module Fetch.ErrorHandling,
   )where

import Prelude hiding (concat, reverse, takeWhile, null)

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import Data.Text
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import System.Directory
import System.FilePath.Posix.Text ((</>), normalise, FilePathT)

import Fetch.Types
import Fetch.ErrorHandling
import System.REPL

-- |Gets the content of an URL.
--  @simpleDownload = withSocketsDo . simpleHttp@ and thus, caveats of
--  @simpleHttp@ apply.
simpleDownload :: URL -> IO BL.ByteString
simpleDownload = withSocketsDo . simpleHttp . unpack

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
   do req' <- catchIO url FormatError $ parseUrl $ unpack url
      let req = req'{method = "GET", requestHeaders = [("User-Agent","Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/36.0.1985.143 Safari/537.36")]}
      res <- catchHttp url $ withSocketsDo $ httpLbs req man
      liftIO $ putStrLnT (url `append` " downloaded.")
      return $ responseBody res

-- |Saves the @ByteString@ (the contents of a response) to a local
--  file, under the filename given in the URL.
saveURL :: FilePathT -> URL -> BL.ByteString -> ErrorIO ()
saveURL savePath url bs =
   do let filename = reverse $ takeWhile (not . ('/'==)) $ reverse url
      guardErr (null filename) [NetworkError url $ FormatError $ "URL '" `append` url `append` " doesn't contain a filename."]
      catchIO url FileError $ createDirectoryIfMissing True (unpack savePath)
      catchIO url FileError $ BL.writeFile (unpack $ savePath </> filename) bs
      return ()

-- |Downloads the contants of a URL and saves them to a given location.
--  This is a convenience wrapper around @download@.
downloadSave :: Manager -> FilePathT -> URL -> ErrorIO ()
downloadSave m fp url = download m url >>= saveURL fp url


-- |Downloads a series of files to given location.
--  In case of error during a download, the function will attempt
--  to continue with the next file.
--  At the end, the list of errors are returned, if there were any.
downloadFiles :: Manager -> FilePathT -> [URL] -> ErrorIO [NetworkError]
downloadFiles m p = mapErr_ (\u -> downloadSave m p u `reportError` print')
   where print' = liftIO . print

-- |Simpler version of @downloadFiles@.
--  Downloads a series of files to a given location.
--  The given path is appended to the user's Downloads directory,
--  assumed to be '<home>/Downloads'.
--  At the end, the list of errors are returned, if there were any.
downloadFiles' :: Manager -> FilePathT -> [URL] -> ErrorIO [NetworkError]
downloadFiles' m p u = do dl <- catchIO "File" FileError downloadsFolder
                          downloadFiles m (dl </> p) u

-- |Gets the user's Downloads folder. This is assumed to be
--  the directory named \"Dowloads\" (case sensitive)
--  in the user's home directory.
downloadsFolder :: IO FilePathT
downloadsFolder = liftM (normalise . (</> "Downloads") . pack) getHomeDirectory

