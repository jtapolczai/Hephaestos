{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Hephaestos where

import Control.Monad.Except
import qualified Data.Map as M
import Network.HTTP.Conduit (newManager)
import Network.HTTP.Client (defaultManagerSettings)
import System.Directory
import System.FilePath.Posix (combine)

import Galleries.List
import Fetch

import CLI

main :: IO ()
main = do st <- runExceptT initState
          case st of Right st' -> mainCLI st'
                     Left err -> mapM_ print err
   where
      initState :: ErrorIO AppState
      initState = do dlf <- catchIO "File" FileError downloadsFolder
                     cur <- catchIO "File" FileError getCurrentDirectory
                     let scd = cur `combine` "scripts/"
                     sc <- comics scd
                     m <- liftIO $ newManager defaultManagerSettings
                     return AppState{wd=dlf,
                                     manager=m,
                                     scriptDir=scd,
                                     linearScripts=sc,
                                     treeScripts=M.empty}