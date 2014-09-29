{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Configuration for the CLI. Manages application-global variables,
--  loading of defaults.
module Crawling.Hephaestos.CLI.Config where

import Prelude hiding ((++))
import qualified Prelude as Pr

import Control.Monad.Except
import Data.Default
import Data.Either.Optional
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified System.FilePath.Posix.Generic as G
import System.IO
import Text.Read (readMaybe)

import Crawling.Hephaestos.Helper.Functor
import Crawling.Hephaestos.Helper.String ((++))


-- |Global Request configuration.
data RequestConfig = RequestConfig {method :: C.Method,
                                    secure :: Bool,
                                    requestHeaders :: [C.Header],
                                    redirectCount :: Int}
   deriving (Show, Eq, Read)

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
   def = AppConfig $ M.fromList [("configFile", "config.txt"),
                                 ("requestConfig", "requestConfig.txt"),
                                 ("scriptDir", "scripts")]

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
   readConfigFile config (toEither (mkErr $ defError config) . readMaybe . T.unpack)
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

-- |Tries to read a configuration from file. If the file is missing,
--  a default instance is written to file and returned. The following
--  exceptions may be thrown:
--  * @IOException@, if the IO operations associated with reading or creating the
--    configuration file fail, and
--  * An exception of type @e@ if the configuration file is present, but its
--    contents can't be parsed.
readConfigFile :: forall a e m.
                  (MonadError e m, Functor m, MonadIO m, Default a, Show a)
               => G.FilePathT -- ^The path of the configuration file.
               -> (T.Text -> Either e a) -- ^Parser for the file's contents.
               -> m a
readConfigFile pathT parser = do
   let path = T.unpack pathT
   liftIO $ createDirectoryIfMissing True path
   exists <- liftIO $ doesFileExist path
   content <- if not exists then do liftIO $ writeFile path (show (def :: a))
                                    return $ Right def
              else liftIO (T.readFile path) >$> parser
   either throwError return content

-- |Tries to read the global request configuration from file.
--  See 'readConfigFile' for error-behaviour.
readRequestConfig :: (MonadError e m, MonadIO m, Functor m)
                  => (T.Text -> e) -> m RequestConfig
readRequestConfig mkErr =
   do filename <- appData mkErr >$> lookupKey "requestConfig"
      let parser = toEither (mkErr $ defError filename) . readMaybe . T.unpack
      readConfigFile filename parser
