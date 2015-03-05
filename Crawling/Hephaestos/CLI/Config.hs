{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- |Configuration for the CLI. Manages application-global variables,
--  loading of defaults.
module Crawling.Hephaestos.CLI.Config (
   -- * Global app configuration
   readAppConfig,
   AppConfig(..),
   configFile,
   requestConfig,
   scriptDir,
   appLang,
   maxFailureNodes,
   createReferer,
   threadPoolSize,
   saveFetchState,
   saveReqMod,
   termWidth,
   minTermHeight,
   useSingleScreen,
   screenUpdateFrequency,
   -- * Request configuration
   readRequestConfig,
   ) where

import Prelude hiding (FilePath)

import Control.Lens (makeLenses, (^.), (%~), (&))
import Control.Monad (mzero)
import Data.Aeson
import Data.CaseInsensitive (mk, original)
import Data.Default
import Data.Functor
import Data.Functor.Monadic
import qualified Data.Semigroup as SG
import qualified Data.Set as S
import qualified Data.Text as TS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C
import qualified Filesystem.Path.CurrentOS as Fp
import System.REPL.Config (readConfigJSON)

import qualified Crawling.Hephaestos.Fetch.Types as FT

-- |Wrapper for Maybe that overrides the JSON instance.
--  @Nothing@ is turned into the string @"null"@, @Just x@ into @toJSON x@.
newtype JMaybe a = JMaybe (Maybe a)

instance ToJSON a => ToJSON (JMaybe a) where
   toJSON (JMaybe Nothing) = "null"
   toJSON (JMaybe (Just x)) = toJSON x

instance FromJSON a => FromJSON (JMaybe a) where
   parseJSON (String "null") = return $ JMaybe Nothing
   parseJSON x = parseJSON x >$> (JMaybe . Just)

instance ToJSON FT.RequestConfig where
   toJSON r = object ["method" .= (JMaybe $ decodeUtf8 <$> FT._method r),
                      "secure" .= FT._secure r,
                      "requestHeaders" .= S.toList (S.map mkHeader (FT._requestHeaders r)),
                      "redirectCount" .= FT._redirectCount r,
                      "requestTimeout" .= FT._requestTimeout r]
      where mkHeader (k,v) = (decodeUtf8 $ original k, decodeUtf8 v)

instance FromJSON FT.RequestConfig where
   parseJSON (Object v) = do
      (JMaybe method) <- v .: "method"
      (JMaybe secure) <- v .: "secure"
      headers <- v .: "requestHeaders"
      (JMaybe count) <- v .: "redirectCount"
      (JMaybe timeout) <- v .: "requestTimeout"
      return $ FT.RequestConfig (encodeUtf8 <$> method)
                                secure
                                (S.fromList $ map mkHeader headers)
                                count
                                timeout
                                []
      where
         mkHeader (k,v) = (mk $ encodeUtf8 k, encodeUtf8 v)
   parseJSON _ = mzero

-- |Global Application configuration.
data AppConfig = AppConfig {_configFile :: Fp.FilePath,
                            _requestConfig :: Fp.FilePath,
                            _scriptDir :: Fp.FilePath,
                            _appLang :: TS.Text,
                            _maxFailureNodes :: Maybe Int,
                            _createReferer :: Bool,
                            _threadPoolSize :: Int,
                            _saveFetchState :: Bool,
                            _saveReqMod :: Bool,
                            _termWidth :: Int,
                            _minTermHeight :: Int,
                            _useSingleScreen :: Bool,
                            _screenUpdateFrequency :: Int
                            }
   deriving (Show, Eq)

makeLenses ''AppConfig

instance Default AppConfig where
   def = AppConfig{_configFile = "config" Fp.</> "config.json",
                   _requestConfig = "config" Fp.</> "requestConfig.json",
                   _scriptDir = "scripts/",
                   _appLang = "en",
                   _maxFailureNodes = Just 3,
                   _createReferer = True,
                   _threadPoolSize = 10,
                   _saveFetchState = True,
                   _saveReqMod = False,
                   _termWidth = 80,
                   _minTermHeight = 10,
                   _useSingleScreen = True,
                   _screenUpdateFrequency = 1000
                   }

instance ToJSON AppConfig where
   toJSON x = object ["configFile" .= Fp.encodeString (_configFile x),
                      "requestConfig" .= Fp.encodeString (_requestConfig x),
                      "scriptDir" .= Fp.encodeString (_scriptDir x),
                      "appLang" .= _appLang x,
                      "maxFailureNodes" .= JMaybe (_maxFailureNodes x),
                      "createReferer" .= True,
                      "threadPoolSize" .= _threadPoolSize x,
                      "saveFetchState" .= _saveFetchState x,
                      "saveReqMod" .= _saveReqMod x,
                      "termWidth" .= _termWidth x,
                      "minTermHeight" .= _minTermHeight x,
                      "useSingleScreen" .= _useSingleScreen x,
                      "screenUpdateFrequency" .= _screenUpdateFrequency x]

instance FromJSON AppConfig where
   parseJSON (Object v) = do
      cf <- v .: "configFile" >$> Fp.decodeString
      rc <- v .: "requestConfig" >$> Fp.decodeString
      sd <- v .: "scriptDir" >$> Fp.decodeString
      lang <- v .: "appLang"
      (JMaybe maxFail) <- v .: "maxFailureNodes"
      cr <- v .: "createReferer"
      tps <- v .: "threadPoolSize"
      sfs <- v .: "saveFetchState"
      srm <- v .: "saveReqMod"
      tw <- v .: "termWidth"
      mth <- v .: "minTermHeight"
      usc <- v .: "useSingleScreen"
      suf <- v .: "screenUpdateFrequency"
      return $ AppConfig cf rc sd lang maxFail cr tps sfs srm tw mth usc suf
   parseJSON _ = mzero

-- |Global configuration strings, read from the the config file.
--  See 'readConfigFile' for error-behaviour.
readAppConfig :: IO AppConfig
readAppConfig = readConfigJSON (_configFile def)

-- |Tries to read the global request configuration from file.
--  See 'readConfigFile' for error-behaviour.
readRequestConfig :: AppConfig -> IO FT.RequestConfig
readRequestConfig config = readConfigJSON (_requestConfig config)
