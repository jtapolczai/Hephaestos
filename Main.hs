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
import Data.Functor.Monadic
import Data.Dynamic
import qualified Data.Map as M
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import Network.Socket (withSocketsDo)
import System.Directory
import Filesystem.Path.CurrentOS' ((</>), decodeString, windowsEscape)

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
         config <- readAppConfig
         req <- readRequestConfig config
         dlf <- downloadsFolder
         cur <- getCurrentDirectory >$> decodeString
         (crawlers :: Crawlers) <- Lib.allCrawlers (config ^. appLang)
                                                   (cur </> (config ^. scriptDir))
         m <- newManager defaultManagerSettings
         tasks <- atomicallyM $ makeCategories [downloadingTasks,
                                                failedTasks,
                                                finishedTasks]
         taskStats <- atomicallyM $ newTVar $ M.fromList [(downloadingTasks, 0),
                                                          (failedTasks, 0),
                                                          (finishedTasks, 0)]
         taskLimit <- atomicallyM $ newTaskLimit $ Just $ config ^. threadPoolSize


         return AppState{pwd=dlf,
                         manager=m,
                         appConfig=config,
                         reqConf=req,
                         crawlers=crawlers,
                         tasks=tasks,
                         taskStats=taskStats,
                         taskLimit=taskLimit,
                         escapingFunction=windowsEscape}
