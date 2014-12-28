{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Configuration for the CLI. Manages application-global variables,
--  loading of defaults.
module Crawling.Hephaestos.CLI.Config where

import Prelude hiding ((++))
import qualified Prelude as Pr

import Control.Arrow
import Control.Monad.Except
import Data.Aeson
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import Data.CaseInsensitive (mk, original)
import Data.Default
import Data.Either.Optional
import Data.Functor.Monadic
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Types.Isomorphic
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C
import System.Directory.Generic (createDirectoryIfMissing, doesFileExist)
import qualified System.FilePath.Generic as G
import System.Directory (getCurrentDirectory)
import System.IO

import Debug.Trace

import Crawling.Hephaestos.Helper.String ((++))

-- |Global Request configuration.
data RequestConfig = RequestConfig {method :: C.Method,
                                    secure :: Bool,
                                    requestHeaders :: [C.Header],
                                    redirectCount :: Int}
   deriving (Show, Eq, Read)

instance ToJSON RequestConfig where
   toJSON r = object ["method" .= decodeUtf8 (method r),
                      "secure" .= secure r,
                      "requestHeaders" .= map mkHeader (requestHeaders r),
                      "redirectCount" .= redirectCount r]
      where mkHeader (k,v) = (decodeUtf8 $ original k, decodeUtf8 v)

instance FromJSON RequestConfig where
   parseJSON (Object v) = do
      method <- v .: "method"
      secure <- v .: "secure"
      headers <- v .: "requestHeaders"
      count <- v .: "redirectCount"
      return $ RequestConfig (encodeUtf8 method) secure (map mkHeader headers) count
      where mkHeader (k,v) = (mk $ encodeUtf8 k, encodeUtf8 v)
   parseJSON _ = error "oh oh"

instance Default RequestConfig where
   def = RequestConfig
         { method = C.method req,
           secure = C.secure req,
           requestHeaders = C.requestHeaders req,
           redirectCount = C.redirectCount req
         }
      where req :: C.Request
            req = def

-- |Global Application configuration.
data AppConfig = AppConfig {fromAppConfig::M.Map T.Text T.Text}
   deriving (Show, Eq, Read)

instance Default AppConfig where
   def = AppConfig $ M.fromList [("configFile", "config" G.</> "config.txt"),
                                 ("requestConfig", "config" G.</> "requestConfig.txt"),
                                 ("scriptDir", "scripts")]

instance ToJSON AppConfig where
   toJSON (AppConfig m) = object $ map mkHeader $ M.toList m
      where
         mkHeader (k,v) = T.toStrict k .= v

instance FromJSON AppConfig where
   parseJSON (Object v) = do keys <- foldM mkKey [] (HM.toList v)
                             return $ AppConfig $ M.fromList keys
      where
         mkKey xs (k,(Ae.String v)) = return $ (T.fromStrict k, T.fromStrict v):xs
         mkKey _ _ = mzero
   parseJSON _ = mzero

--instance FromJSon AppConfig
--   parseJSON (Object v) =
--   parseJSON _ = mzero

-- |Convenience function for looking up keys in the application configuration.
--  @
--  lookupKey k (AppConfig c) = fromJust $ lookup k c
--  @
lookupKey :: T.Text -> AppConfig -> T.Text
lookupKey k = (M.! k) . fromAppConfig

-- |Global configuration strings, read from the the config file.
--  See 'readConfigFile' for error-behaviour.
appData :: (MonadError e m, MonadIO m, Functor m) => (T.Text -> e) -> m AppConfig
appData mkErr =
   readConfigFile config (toEither (mkErr $ defError config) . decode')
   where config = lookupKey "configFile" def

-- |The default error message if the parsing of a file fails.
defError :: T.Text -> T.Text
defError x = "Couldn't parse " ++ x ++ "! Correct the file or delete it to " ++
             "restore the defaults. If this message persists, the application " ++
             "has a bug."

-- |Turns a 'RequestConfig' into a function that modifies 'Request's.
runRequestConfig :: RequestConfig -> C.Request -> C.Request
runRequestConfig conf req =
   req{C.method = method conf,
       C.secure = secure conf,
       C.redirectCount = redirectCount conf,
       C.requestHeaders = C.requestHeaders req Pr.++ requestHeaders conf}

-- |Tries to read the global request configuration from file.
--  See 'readConfigFile' for error-behaviour.
readRequestConfig :: (MonadError e m, MonadIO m, Functor m)
                  => AppConfig -> (T.Text -> e) -> m RequestConfig
readRequestConfig config mkErr = readConfigFile filename parser
   where
      filename = lookupKey "requestConfig" config
      parser = toEither (mkErr $ defError filename) . decode'

-- |Tries to read a configuration from file. If the file is missing,
--  a default instance is written to file and returned. The following
--  exceptions may be thrown:
--  * @IOException@, if the IO operations associated with reading or creating the
--    configuration file fail, and
--  * An exception of type @e@ if the configuration file is present, but its
--    contents can't be parsed.
readConfigFile :: forall a e m f.
                  (MonadError e m, Functor m, MonadIO m, Default a, ToJSON a,
                   Injective f String)
               => f -- ^The path of the configuration file.
               -> (BL.ByteString -> Either e a) -- ^Parser for the file's contents.
               -> m a
readConfigFile pathT parser = do
   let path = to pathT
   createDirectoryIfMissing True $ (G.dropFileName path :: String)
   exists <- liftIO $ doesFileExist path
   content <- if not exists then do liftIO $ BL.writeFile path (encode (def :: a))
                                    return $ Right def
              else liftIO (BL.readFile path) >$> parser
   either throwError return content
