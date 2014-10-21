{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |Main module of the CLI.
module Hephaestos where

import Control.Exception
import Control.Monad.Except
import Data.Functor.Monadic
import Data.HList.HList
import qualified Data.Map as M
import Data.Text (pack, Text)
import Data.Void
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

pk :: (ConfigurableCrawler c ErrorIO' b a, StateCrawler c ErrorIO' b a) =>
      HList FetchTreeArgs
      -> c ErrorIO' b a
      -> b
      -> a
      -> (MTree ErrorIO' (SuccessorNode SomeException a))
pk (HCons m (HCons req (HCons url HNil))) cr config state =
   fetchTree m (crawlerFunction cr config) req state url

pkLin :: HList FetchTreeArgs
         -> SimpleLinearCrawler m Void (Maybe Int)
         -> Void
         -> Maybe Int
         -> (MTree ErrorIO' (SuccessorNode SomeException a))
pkLin = undefined

-- ------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-------------------------
--Make SimpleLinearCrawler into a State/Config crawler

-- |The entry point for the CLI.
main :: IO ()
main = do st <- runExceptT initState
          case st of Right st' -> mainCLI st'
                     Left err -> printError err
   where
      mkErr = SomeException . NetworkError "File" . FormatError

      trees = M.fromList [("fileList" :: Text, packCrawler fileListC pk)]
         where
            fileListC = configCrawler "fileList" "http://*" fileList'
                        (ask' fileAsk)
               where fileAsk = asker "Enter number of items: "
                                     ("Expected positive integer!" :: Text)
                                     "Expected positive integer!"
                                     (return . (>0))

      initState :: ErrorIO AppState
      initState = do config <- appData mkErr
                     let scriptDir = lookupKey "scriptDir" config
                     req <- readRequestConfig config mkErr >$> runRequestConfig
                     dlf <- catchIO "File" FileError downloadsFolder
                     cur <- catchIO "File" FileError getCurrentDirectory
                     trees' <- comics (pack cur </> scriptDir)
                               >$> M.map (`packCrawler` pk)
                               >$> M.union trees
                     m <- liftIO $ newManager defaultManagerSettings
                     return AppState{pwd=dlf,
                                     manager=m,
                                     appConfig=config,
                                     reqMod=req,
                                     treeScripts=trees}
