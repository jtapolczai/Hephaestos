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

-- |Global configuration strings.
appData :: (MonadError T.Text m, MonadIO m, Functor m) => m AppConfig
appData = readConfigFile config (toEither (defError config) . readMaybe . T.unpack)
   where config = lookupKey "configFile" def

-- |The default error message if the parsing of a file fails.
defError :: T.Text -> T.Text
defError x = "Couldn't parse " ++ x ++ "! Correct the file or delete it to " ++
             "restore the defaults. If this message persists, the application " ++
             "has a bug."

-- |Turns a 'RequestConfig' into a function that modifies 'Request's.
runRequestConfig :: RequestConfig -> C.Request -> C.Request
runRequestConfig conf req = req{C.method = method conf,
                                C.secure = secure conf,
                                C.redirectCount = redirectCount conf,
                                C.requestHeaders = C.requestHeaders req Pr.++
                                                   requestHeaders conf}

-- |Tries to read a configuration from file. If the file is missing,
--  a default instance is written to file and returned. If the file
--  exists but can't be parsed, or if any IO operation fails, an exception
--  is thrown.
readConfigFile :: forall a e m. (MonadError e m, Functor m,
                                 MonadIO m, Default a, Show a) =>
               G.FilePathT -> (T.Text -> Either e a) -> m a
readConfigFile pathT parser = do
   let path = T.unpack pathT
   liftIO $ createDirectoryIfMissing True path
   exists <- liftIO $ doesFileExist path
   content <- if not exists then do liftIO $ writeFile path (show (def :: a))
                                    return $ Right def
              else liftIO (T.readFile path) >$> parser
   either throwError return content

-- |Tries to read the global request configuration from file.
--  If it's missing, a default configuration is written and returned.
--
--  Exceptions are thrown in the following two cases:
--  *  If a configuration is present but can't be read due to errors, or
--  *  if a configuration is not present and can't be created.
--readRequestConfig :: (MonadError NetworkError m, MonadIO m) => m RequestConfig
readRequestConfig :: (MonadError T.Text m, MonadIO m, Functor m)
                  => m RequestConfig
readRequestConfig = do filename <- appData >$> lookupKey "requestConfig"
                       let parser = toEither (defError filename)
                                    . readMaybe . T.unpack
                       readConfigFile filename parser
