{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- |Main module of the CLI.
module Hephaestos where

import Control.Monad.Except
import Data.Functor.Monadic
import Data.HList.HList
import qualified Data.Map as M
import Data.Text (pack, Text)
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import System.Directory
import System.FilePath.Generic ((</>))
import System.REPL

import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Crawlers.Templates
import Crawling.Hephaestos.Crawlers.Linear.Load
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.CLI
import Crawling.Hephaestos.CLI.Config

pk :: HList FetchTreeArgs -> TreeCrawler ErrorIO' b a -> b -> a -> (MTree ErrorIO' (SuccessorNode [NetworkError] a))
pk (HCons m (HCons req (HCons url HNil))) cr config state = fetchTree m (tcSucc cr config) req state url

-- |The entry point for the CLI.
main :: (MonadError (AskFailure Text) IO,
         MonadError (AskFailure Text) ErrorIO',
         Show (AskFailure Text)) => IO ()
main = do st <- runExceptT initState
          case st of Right st' -> mainCLI st'
                     Left err -> mapM_ printError err
   where
      mkErr = (:[]) . NetworkError "File" . FormatError

      trees = M.fromList [("fileList" :: Text, packCrawler fileListC pk)]
         where
            fileListC = configCrawler "fileList" "http://*" fileList'
                        (ask' $ asker "Enter number of items: "
                                      ("Expected positive integer!" :: Text)
                                      "Expected positive integer!"
                                      (return . (>0)))

      initState :: ErrorIO AppState
      initState = do config <- appData mkErr
                     let scriptDir = lookupKey "scriptDir" config
                     req <- readRequestConfig config mkErr >$> runRequestConfig
                     dlf <- catchIO "File" FileError downloadsFolder
                     cur <- catchIO "File" FileError getCurrentDirectory
                     let scd = pack cur </> scriptDir
                     sc <- comics scd
                     m <- liftIO $ newManager defaultManagerSettings
                     return AppState{pwd=dlf,
                                     manager=m,
                                     appConfig=config,
                                     reqMod=req,
                                     linearScripts=sc,
                                     treeScripts=trees}
