{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- |Configuration for the CLI. Manages application-global variables,
--  loading of defaults.
module Crawling.Hephaestos.CLI.Config where

import Prelude hiding ((++), FilePath)
import qualified Prelude as Pr

import Control.Arrow
import Control.Lens (makeLenses, (^.))
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
data RequestConfig = RequestConfig {_method :: C.Method,
                                    _secure :: Bool,
                                    _requestHeaders :: [C.Header],
                                    _redirectCount :: Int,
                                    _createReferer :: Bool}
   deriving (Show, Eq, Read)

makeLenses ''RequestConfig

instance ToJSON RequestConfig where
   toJSON r = object ["method" .= decodeUtf8 (_method r),
                      "secure" .= _secure r,
                      "requestHeaders" .= map mkHeader (_requestHeaders r),
                      "redirectCount" .= _redirectCount r,
                      "createReferer" .= _createReferer r]
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
         { _method = C.method req,
           _secure = C.secure req,
           _requestHeaders = C.requestHeaders req,
           _redirectCount = C.redirectCount req,
           _createReferer = True
         }
      where req :: C.Request
            req = def

-- |Global Application configuration.
data AppConfig = AppConfig {_configFile :: Fp.FilePath,
                            _requestConfig :: Fp.FilePath,
                            _scriptDir :: Fp.FilePath,
                            _maxFailureNodes :: Maybe Int,
                            _appLang :: TS.Text,
                            _saveFetchState :: Bool,
                            _saveReqMod :: Bool}
   deriving (Show, Eq)

makeLenses ''AppConfig

-- |Wrapper for Maybe that overrides the JSON instance.
--  @Nothing@ is turned into the string @"null"@, @Just x@ into @toJSON x@.
newtype JMaybe a = JMaybe (Maybe a)

instance ToJSON a => ToJSON (JMaybe a) where
   toJSON (JMaybe Nothing) = "null"
   toJSON (JMaybe (Just x)) = toJSON x

instance FromJSON a => FromJSON (JMaybe a) where
   parseJSON (String "null") = return $ JMaybe Nothing
   parseJSON x = parseJSON x >$> (JMaybe . Just)

instance Default AppConfig where
   def = AppConfig{_configFile = "config" Fp.</> "config.json",
                   _requestConfig = "config" Fp.</> "requestConfig.json",
                   _scriptDir = "scripts/",
                   _maxFailureNodes = Just 3,
                   _appLang = "en",
                   _saveFetchState = True,
                   _saveReqMod = False}

instance ToJSON AppConfig where
   toJSON x = object $ ["configFile" .= Fp.encodeString (_configFile x),
                        "requestConfig" .= Fp.encodeString (_requestConfig x),
                        "scriptDir" .= Fp.encodeString (_scriptDir x),
                        "maxFailureNodes" .= JMaybe (_maxFailureNodes x),
                        "appLang" .= _appLang x,
                        "saveFetchState" .= _saveFetchState x,
                        "saveReqMod" .= _saveReqMod x]

instance FromJSON AppConfig where
   parseJSON (Object v) = do
      cf <- v .: "configFile" >$> Fp.decodeString
      rc <- v .: "requestConfig" >$> Fp.decodeString
      sd <- v .: "scriptDir" >$> Fp.decodeString
      lang <- v .: "appLang"
      (JMaybe maxFail) <- v .: "maxFailureNodes"
      sfs <- v .: "saveFetchState"
      srm <- v .: "saveReqMod"
      return $ AppConfig cf rc sd maxFail lang sfs srm
   parseJSON _ = mzero

-- |Global configuration strings, read from the the config file.
--  See 'readConfigFile' for error-behaviour.
appData :: IO AppConfig
appData = readConfigJSON (_configFile def)

-- |Tries to read the global request configuration from file.
--  See 'readConfigFile' for error-behaviour.
readRequestConfig :: AppConfig -> IO RequestConfig
readRequestConfig config = readConfigJSON (_requestConfig config)

-- |The default error message if the parsing of a file fails.
defError :: Fp.FilePath -> T.Text
defError x = "Couldn't parse " `append` x' `append` "! Correct the file or delete it to " `append`
             "restore the defaults. If this message persists, the application " `append`
             "has a bug."
   where x' = either T.fromStrict T.fromStrict $ Fp.toText x

-- |Turns a 'RequestConfig' into a function that modifies 'Request's.
runRequestConfig :: RequestConfig -> C.Request -> C.Request
runRequestConfig conf req =
   req{C.method = conf ^. method,
       C.secure = conf ^. secure,
       C.redirectCount = conf ^. redirectCount,
       C.requestHeaders = C.requestHeaders req Pr.++ conf ^. requestHeaders}
