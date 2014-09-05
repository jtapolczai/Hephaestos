{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Fetch where

import Prelude hiding (concat)

import Control.Exception (catches, IOException, Handler(..))
import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import Data.Either (lefts)
import Data.List (foldl')
import Data.Monoid
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import System.Directory
import System.FilePath
import Text.XML (Document)
import Text.XML.Cursor

type URL = String

type HTTPStatus = Int
data NetworkError = ConnectionError String
                    | StatusError HTTPStatus String
                    | FileError String
                    | FormatError String
                    | DataFindingError String

type ErrorIO a = ExceptT [NetworkError] IO a

instance Show NetworkError where
   show (ConnectionError m) = m
   show (StatusError s m) = show s ++ ": " ++ m
   show (FileError m) = m
   show (FormatError m) = m
   show (DataFindingError m) = m

fromIOException :: (String -> NetworkError) -> IOException -> NetworkError
fromIOException f = f . show

-- |Lifts an IO action into the ExceptT IO transformer,
--  transforming all thrown IOExceptions into NetworkErrors.
catchIO :: (String -> NetworkError) -> IO a -> ErrorIO a
catchIO e m = liftIO m' >>= \x -> case x of (Right r) -> return r
                                            (Left l) -> throwError [l]
   where
      m' = liftM Right m `catches` [Handler (\(ex :: IOException) -> return $! Left $ e $ show ex)]

addError :: MonadError [e] m => e -> m a
addError e = throwError [e]

printError :: NetworkError -> ErrorIO ()
printError = liftIO . print

-- |Collects the errors from a list of results.
--  Defined as @return . mconcat . lefts@.
collectErrors :: (Monoid e, Monad m) => [Either e a] -> m e
collectErrors = return . mconcat . lefts

-- |Catches an error, performs an action, and re-throws it.
reportError :: MonadError e m => m a -> (e -> m b) -> m a
reportError m f = m `catchError` (\e -> f e >> throwError e)

guardErr :: MonadError e m => Bool -> e -> m ()
guardErr True = throwError
guardErr False = const $ return ()

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- |Version of @mapM_@ which collects all the errors which occur.
mapErr_ :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m e
mapErr_ f = mapErr f >=> collectErrors

-- |Version of @mapM@ which returns either the result of the function
--  or the error which occurred during its evaluation.
mapErr :: (Monoid e, MonadError e m) => (a -> m b) -> [a] -> m [Either e b]
mapErr _ [] = return []
mapErr f (x:xs) = liftM2 (:) (liftM Right (f x) `catchError` (return . Left))
                             (mapErr f xs)

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

-- |Pads a list cs to a length of i with filler elements c such that
--  @padLeft c i cs = cc++cs@ with @cc = [c,...,c]@.
padLeft :: a -> Int -> [a] -> [a]
padLeft c i cs = replicate (i - length cs) c ++ cs

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


-- |Runs an XPath expression against a document tree.
runXPath :: Document -> (Cursor -> a) -> a
runXPath doc xpath = fromDocument doc $| xpath

-- |The []-predicate of XPath. Can be applied to an @Axis@
--  with @>=>@, e.g. @r $// element "div" >=> at 4@.
at :: Int -> Axis
at i = foldl' (>=>) (:[]) (replicate i followingSibling)

dlFolder :: IO FilePath
dlFolder = liftM (</> "Downloads") getHomeDirectory
