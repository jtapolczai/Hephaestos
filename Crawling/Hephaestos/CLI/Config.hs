-- |Configuration for the CLI. Manages application-global variables,
--  loading of defaults.
module Crawling.Hephaestos.CLI.Config where

import Data.Default
import Data.Either.Optional
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Conduit as C
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified System.FilePath.Posix.Generic as G
import System.IO
import Text.Read (readMaybe)


-- |Global Request configuration.
data RequestConfig = RequestConfig {method :: Method,
                                    secure :: Bool,
                                    requestHeaders :: [Header],
                                    redirectCount :: Int}
   deriving (Show, Eq, Read)

instance Default RequestConfig where
   def = RequestConfig
         { method = C.method req,
           secure = C.secure req,
           requestHeaders = C.requestHeaders req,
           redirectCount = C.redirectCount req
         }
      where req :: Request
            req = def

-- |Global Application configuration.
data AppConfig = AppConfig {fromAppConfig::M.Map Text Text}
   deriving (Show, Eq, Read)

instance Default AppConfig where
   def = AppConfig $ M.fromList [("configFile", "config.txt"),
                                 ("requestConfig", "requestConfig.txt")]

-- |Global configuration strings.
appData :: IO AppConfig
appData = readConfigFile (M.lookup "configFile" $ fromAppConfig $ def)
          (\x -> toEither "Couldn't parse config.txt!" $ readMaybe x)

-- |Turns a 'RequestConfig' into a function that modifies 'Request's.
runRequestConfig :: RequestConfig -> Request -> Request
runRequestConfig = undefined

-- |Tries to read a configuration from file. If the file is missing,
--  a default instance is written to file and returned. If the file
--  exists but can't be parsed, or if any IO operation fails, an exception
--  is thrown.
readConfigFile :: (MonadError e m, MonadIO m, Default a, Show a) =>
               FilePathT -> (Text -> Either e a) -> m a
readConfigFile pathT parser = do
   let path = T.unpack path
   liftIO $ createDirectoryIfMissing True path
   exists <- liftIO $ doesFileExist path
   content <- if not exists then do liftIO $ writeFile path (show def :: a)
                                    return $ Right def
              else T.readFile path >$> parser
   either throwError return content

-- |Tries to read the global request configuration from file.
--  If it's missing, a default configuration is written and returned.
--
--  Exceptions are thrown in the following two cases:
--  *  If a configuration is present but can't be read due to errors, or
--  *  if a configuration is not present and can't be created.
readRequestConfig :: (MonadError NetworkError m, MonadIO m) => m RequestConfig
readRequestConfig = do filename <- appData >$> fromAppConfig
                                           >$> M.lookup "requestConfig"
                       let parser x = toEither
                                      ("Couldn't parse " ++ filename ++ "!")
                                      $ readMaybe x
                       readConfigFile filename parser
