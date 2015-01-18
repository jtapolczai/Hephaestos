{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Configuration for the CLI. Manages application-global variables,
--  loading of defaults.
module Crawling.Hephaestos.CLI.Config where

import Prelude hiding ((++), FilePath)
import qualified Prelude as Pr

import Control.Arrow
import Control.Monad (mzero)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive (mk, original)
import Data.Default
import Data.Functor.Monadic
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.ListLike (ListLike(append), StringLike(fromString))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Types.Isomorphic
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C
import qualified Filesystem.Path.CurrentOS as Fp
import System.Directory
import System.IO hiding (FilePath)
import Text.Read (readMaybe)

import Debug.Trace

-- |Global Request configuration.
data RequestConfig = RequestConfig {method :: C.Method,
                                    secure :: Bool,
                                    requestHeaders :: [C.Header],
                                    redirectCount :: Int,
                                    createReferer :: Bool}
   deriving (Show, Eq, Read)

instance ToJSON RequestConfig where
   toJSON r = object ["method" .= decodeUtf8 (method r),
                      "secure" .= secure r,
                      "requestHeaders" .= map mkHeader (requestHeaders r),
                      "redirectCount" .= redirectCount r,
                      "createReferer" .= createReferer r]
      where mkHeader (k,v) = (decodeUtf8 $ original k, decodeUtf8 v)

instance FromJSON RequestConfig where
   parseJSON (Object v) = do
      method <- v .: "method"
      secure <- v .: "secure"
      headers <- v .: "requestHeaders"
      count <- v .: "redirectCount"
      referer <- v .: "createReferer"
      return $ RequestConfig (encodeUtf8 method)
                             secure
                             (map mkHeader headers)
                             count
                             referer
      where
         mkHeader (k,v) = (mk $ encodeUtf8 k, encodeUtf8 v)
   parseJSON _ = mzero

instance Default RequestConfig where
   def = RequestConfig
         { method = C.method req,
           secure = C.secure req,
           requestHeaders = C.requestHeaders req,
           redirectCount = C.redirectCount req,
           createReferer = True
         }
      where req :: C.Request
            req = def

-- |Global Application configuration.
data AppConfig = AppConfig {configFile::Fp.FilePath,
                            requestConfig::Fp.FilePath,
                            scriptDir::Fp.FilePath,
                            maxFailureNodes::Maybe Int}
   deriving (Show, Eq)

instance Default AppConfig where
   def = AppConfig{configFile = "config" Fp.</> "config.json",
                   requestConfig = "config" Fp.</> "requestConfig.json",
                   scriptDir = "scripts/",
                   maxFailureNodes = Just 3}

instance ToJSON AppConfig where
   toJSON x = object ["configFile" .= Fp.encodeString (configFile x),
                      "requestConfig" .= Fp.encodeString (requestConfig x),
                      "scriptDir" .= Fp.encodeString (scriptDir x),
                      "maxFailureNodes" .= maybe "null" show (maxFailureNodes x)]

instance FromJSON AppConfig where
   parseJSON (Object v) = do
      cf <- v .: "configFile" >$> Fp.decodeString
      rc <- v .: "requestConfig" >$> Fp.decodeString
      sd <- v .: "scriptDir" >$> Fp.decodeString
      maxFail <- v .: "maxFailureNodes"
      maxFail' <- if maxFail == "null" then return Nothing
                  else maybe mzero (return.Just) (readMaybe maxFail :: Maybe Int)
      return $ AppConfig cf rc sd maxFail'
   parseJSON _ = mzero

-- |Global configuration strings, read from the the config file.
--  See 'readConfigFile' for error-behaviour.
appData :: (MonadThrow m, MonadIO m, Functor m, Exception e) => (T.Text -> e) -> m AppConfig
appData mkErr =
   readConfigFile (configFile def) (maybe (Left $ mkErr $ defError (configFile def)) Right . decode')

-- |The default error message if the parsing of a file fails.
defError :: Fp.FilePath -> T.Text
defError x = "Couldn't parse " `append` x' `append` "! Correct the file or delete it to " `append`
             "restore the defaults. If this message persists, the application " `append`
             "has a bug."
   where x' = either T.fromStrict T.fromStrict $ Fp.toText x

-- |Turns a 'RequestConfig' into a function that modifies 'Request's.
runRequestConfig :: RequestConfig -> C.Request -> C.Request
runRequestConfig conf req =
   req{C.method = method conf,
       C.secure = secure conf,
       C.redirectCount = redirectCount conf,
       C.requestHeaders = C.requestHeaders req Pr.++ requestHeaders conf}

-- |Tries to read the global request configuration from file.
--  See 'readConfigFile' for error-behaviour.
readRequestConfig :: (MonadThrow m, MonadIO m, Functor m, Exception e)
                  => AppConfig -> (T.Text -> e) -> m RequestConfig
readRequestConfig config mkErr = readConfigFile (requestConfig config) parser
   where
      parser = maybe (Left $ mkErr $ defError $ requestConfig config) Right . decode'

-- |Tries to read a configuration from file. If the file is missing,
--  a default instance is written to file and returned. The following
--  exceptions may be thrown:
--  * @IOException@, if the IO operations associated with reading or creating the
--    configuration file fail, and
--  * An exception of type @e@ if the configuration file is present, but its
--    contents can't be parsed.
readConfigFile :: forall e m a.(MonadThrow m, Functor m, MonadIO m, Default a, ToJSON a, Exception e)
               => Fp.FilePath -- ^The path of the configuration file.
               -> (BL.ByteString -> Either e a) -- ^Parser for the file's contents.
               -> m a
readConfigFile path parser = do
   let pathT = Fp.encodeString path
   liftIO $ createDirectoryIfMissing True $ Fp.encodeString $ Fp.parent path
   exists <- liftIO $ doesFileExist $ Fp.encodeString path
   content <- if not exists then do liftIO $ BL.writeFile pathT (encode (def :: a))
                                    return $ Right def
              else liftIO (BL.readFile pathT) >$> parser
   either throwM return content
