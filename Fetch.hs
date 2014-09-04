{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Prelude hiding (concat)

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.Socket.Internal
import System.Directory
import System.FilePath
import Text.XML (Document)
import Text.XML.Cursor

type URL = String

-- |Gets the content of an URL.
--  @simpleDownload = withSocketsDo . simpleHttp@ and thus, caveats of
--  @simpleHttp@ apply.
simpleDownload :: URL -> IO BL.ByteString
simpleDownload = withSocketsDo . simpleHttp

-- |Augments a function which takes a @Manager@ to one that
--  optionally takes one and returns it too.
--  If no manager is supplied, a new one is created and returned.
withManager :: (Manager -> a -> IO b) -> Maybe Manager -> a -> IO (Manager, b)
withManager f m x =
   do m' <- case m of Nothing  -> newManager defaultManagerSettings
                      Just m'' -> return m''
      ret <- f m' x
      return (m',ret)

-- |Downloads the contents of a URL and saves them to
--  a given location. If an error occurs, Left is returned.
--  The @Manager@-parameter is there to enable connection pooling.
download :: Manager -> URL -> IO (Either String BL.ByteString)
download man url =
   do req' <- parseUrl url
      let req = req'{method = "GET"}
      res <- withSocketsDo $ httpLbs req man
      putStrLn (url ++ " downloaded!")
      return $! Right (responseBody res)
      `catches`
      [Handler (\(ex :: IOException) -> return (Left $! show ex))]

-- |Saves the @ByteString@ (the contents of a response) to a local
--  file, under the filename given in the URL.
saveURL :: FilePath -> URL -> BL.ByteString -> IO (Either String ())
saveURL savePath url bs =
   do let filename = reverse $ takeWhile (not . ('/'==)) $ reverse url
      if null filename then return $! Left "empty filename!"
      else do createDirectoryIfMissing True savePath
              BL.writeFile (savePath </> filename) bs
              return $! Right ()

-- |Downloads the contants of a URL and saves them to a given location.
--  This is a convenience wrapper around @download@.
downloadSave :: Maybe Manager -> FilePath -> URL -> IO (Manager, Either String ())
downloadSave m fp url =
   do (m',res) <- withManager download m url
      case res of Left l -> return (m', Left l)
                  Right bs -> do res' <- saveURL fp url bs
                                 return (m',res')

-- |Pads a list cs to a length of i with filler elements c such that
--  @padLeft c i cs = cc++cs@ with @cc = [c,...,c]@.
padLeft :: a -> Int -> [a] -> [a]
padLeft c i cs = replicate (i - length cs) c ++ cs

-- |Downloads a series of files to given location.
downloadFiles :: FilePath -> [URL] -> IO ()
downloadFiles p = foldM_ save Nothing
   where
      save m u = do (m',r) <- downloadSave m p u
                    case r of Left l -> putStrLn l
                              Right _ -> return ()
                    return $! Just m'


-- |Runs an XPath expression against a document tree.
runXPath :: Document -> (Cursor -> a) -> a
runXPath doc xpath = fromDocument doc $| xpath

-- |The []-predicate of XPath. Can be applied to an @Axis@
--  with @>=>@, e.g. @r $// element "div" >=> at 4@.
at :: Int -> Axis
at i = foldl' (>=>) (:[]) (replicate i followingSibling)
