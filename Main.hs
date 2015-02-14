{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |Main module of the CLI.
module Main where

import Control.Concurrent.STM
import Control.Concurrent.STM.Utils
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Functor.Monadic
import Data.Dynamic
import qualified Data.IntMap as IM
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import Network.Socket (withSocketsDo)
import System.Directory
import Filesystem.Path.CurrentOS' ((</>), decodeString)

import Crawling.Hephaestos.CLI.Errors
import qualified Crawling.Hephaestos.Crawlers.Library as Lib
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.CLI
import Crawling.Hephaestos.CLI.Config

type Crawlers = [Lib.ResultSet Lib.Ident [] Dynamic]

-- |The entry point for the CLI.
main :: IO ()
main = withSocketsDo $
   (initState >>= mainCLI) `catchAll` (errorMsg "en" >=> printError)
   where
      initState :: IO AppState
      initState = do
         config <- appData
         req <- readRequestConfig config
         dlf <- downloadsFolder
         cur <- getCurrentDirectory >$> decodeString
         (crawlers :: Crawlers) <- Lib.allCrawlers (config ^. appLang)
                                                   (cur </> (config ^. scriptDir))
         m <- newManager defaultManagerSettings
         tasks <- liftIO $ atomically $ newTVar IM.empty
         taskLimit <- liftIO $ atomically $ newTaskLimit $ Just $ config ^. threadPoolSize


         return AppState{pwd=dlf,
                         manager=m,
                         appConfig=config,
                         reqConf=req,
                         crawlers=crawlers,
                         taskLimit=taskLimit,
                         tasks=tasks}
