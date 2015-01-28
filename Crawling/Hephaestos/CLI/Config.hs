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
import Data.Maybe (fromJust, isJust)
import Data.ListLike (ListLike(append), StringLike(fromString))
import qualified Data.Text as TS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Types.Isomorphic
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C
import qualified Filesystem.Path.CurrentOS as Fp
import System.Directory
import System.IO hiding (FilePath)
import System.REPL.Config (readConfigJSON)
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
                            maxFailureNodes::Maybe Int,
                            appLang :: TS.Text}
   deriving (Show, Eq)

instance Default AppConfig where
   def = AppConfig{configFile = "config" Fp.</> "config.json",
                   requestConfig = "config" Fp.</> "requestConfig.json",
                   scriptDir = "scripts/",
                   maxFailureNodes = Just 3,
                   appLang = "en"}

instance ToJSON AppConfig where
   toJSON x = object $ ["configFile" .= Fp.encodeString (configFile x),
                        "requestConfig" .= Fp.encodeString (requestConfig x),
                        "scriptDir" .= Fp.encodeString (scriptDir x),
                        "appLang" .= appLang x]
                        `append`
                        (maybe [] (\x -> ["maxFailureNodes" .= x]) $ maxFailureNodes x)

instance FromJSON AppConfig where
   parseJSON (Object v) = do
      cf <- v .: "configFile" >$> Fp.decodeString
      rc <- v .: "requestConfig" >$> Fp.decodeString
      sd <- v .: "scriptDir" >$> Fp.decodeString
      lang <- v .: "appLang"
      maxFail <- v .:? "maxFailureNodes"
      return $ AppConfig cf rc sd maxFail lang
   parseJSON _ = mzero

-- |Global configuration strings, read from the the config file.
--  See 'readConfigFile' for error-behaviour.
appData :: IO AppConfig
appData = readConfigJSON (configFile def)

-- |Tries to read the global request configuration from file.
--  See 'readConfigFile' for error-behaviour.
readRequestConfig :: AppConfig -> IO RequestConfig
readRequestConfig config = readConfigJSON (requestConfig config)

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
