{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |Main module of the CLI.
module Main where

import Control.Exception
import Control.Monad.Catch
import Data.Functor.Monadic
import Data.Dynamic
import qualified Data.Map as M
import Data.Text.Lazy (pack, Text)
import Data.Void
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import System.Directory
import System.Directory.Generic (fromText')
import Filesystem.Path.CurrentOS ((</>), decodeString)
import System.REPL

import Crawling.Hephaestos.Crawlers
import qualified Crawling.Hephaestos.Crawlers.Templates as T
import qualified Crawling.Hephaestos.Crawlers.Library as Lib
import Crawling.Hephaestos.Fetch hiding (manager)
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Successor
import Crawling.Hephaestos.CLI
import Crawling.Hephaestos.CLI.Config

type Crawlers = [Lib.ResultSet [] Dynamic]

-- |The entry point for the CLI.
main :: IO ()
main = (initState >>= mainCLI) `catchAll` printError
   where
      initState :: IO AppState
      initState = do config <- appData dataFormatError'
                     req <- readRequestConfig config dataFormatError'
                     dlf <- downloadsFolder
                     cur <- getCurrentDirectory >$> decodeString
                     (crawlers :: Crawlers) <- Lib.allCrawlers (cur </> scriptDir config)
                     m <- newManager defaultManagerSettings

                     return AppState{pwd=dlf,
                                     manager=m,
                                     appConfig=config,
                                     reqConf=req,
                                     crawlers=crawlers}
