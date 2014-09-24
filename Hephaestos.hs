{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Main module of the CLI.
module Hephaestos where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Text (pack)
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import System.Directory
import System.FilePath.Posix.Generic ((</>))

import Crawling.Hephaestos.Crawlers.Linear.Load
import Crawling.Hephaestos.Fetch

import Crawling.Hephaestos.CLI

-- |The entry point for the CLI.
main :: IO ()
main = do st <- runExceptT initState
          case st of Right st' -> mainCLI st'
                     Left err -> mapM_ print err
   where
      initState :: ErrorIO AppState
      initState = do dlf <- catchIO "File" FileError downloadsFolder
                     cur <- catchIO "File" FileError getCurrentDirectory
                     let scd = pack cur </> "scripts/"
                     sc <- comics scd
                     m <- liftIO $ newManager defaultManagerSettings
                     return AppState{pwd=dlf,
                                     manager=m,
                                     scriptDir=scd,
                                     linearScripts=sc,
                                     treeScripts=M.empty}
