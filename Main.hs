{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |Main module of the CLI.
module Main where

import Control.Exception
import Control.Monad.Except
import Data.Functor.Monadic
import Data.HList.HList
import Data.Dynamic
import qualified Data.Map as M
import Data.Text.Lazy (pack, Text)
import Data.Void
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import System.Directory
import System.FilePath.Generic ((</>))
import System.REPL

import Crawling.Hephaestos.Crawlers
import qualified Crawling.Hephaestos.Crawlers.Templates as T
import qualified Crawling.Hephaestos.Crawlers.Library as Lib
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.CLI
import Crawling.Hephaestos.CLI.Config

type Crawlers = [Lib.ResultSet [] Dynamic]

-- |The entry point for the CLI.
main :: IO ()
main = do st <- runExceptT initState
          case st of Right st' -> mainCLI st'
                     Left err -> printError err
   where
      mkErr = SomeException . NetworkError "File" . FormatError

      initState :: ErrorIO AppState
      initState = do config <- appData mkErr
                     let scriptDir = lookupKey "scriptDir" config
                     req <- readRequestConfig config mkErr
                     dlf <- catchIO "File" FileError downloadsFolder
                     cur <- catchIO "File" FileError getCurrentDirectory
                     (crawlers :: Crawlers) <- Lib.allCrawlers (pack cur </> scriptDir)
                     m <- liftIO $ newManager defaultManagerSettings

                     return AppState{pwd=dlf,
                                     manager=m,
                                     appConfig=config,
                                     reqConf=req,
                                     crawlers=crawlers}
