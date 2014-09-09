{- |Provides simple-to-use functionality for large-scale downloading
    jobs.
-}
module Fetch.Job where

import Control.Monad.Except (liftIO)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Conduit

import Fetch.Types

-- |Executes a simple download job: takes a list of URLS
--  (which may be the result of IO) and a saving function
--  and applies the latter to the former.
--  This function creates and closes a manager.
downloadJob :: (Manager -> ErrorIO [URL])
            -> (Manager -> [URL] -> ErrorIO a)
            -> ErrorIO a
downloadJob src save =
   do m <- liftIO $ newManager defaultManagerSettings
      src' <- src m
      ret <- save m src'
      liftIO $ closeManager m
      return ret
