{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

-- |Crawlers for linear webcomics.
module Galleries.List where

import Control.Arrow
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import System.Directory
import System.FilePath.Posix (combine)
import Text.Read (readMaybe)


import Galleries.Linear
import Fetch.ErrorHandling
import Fetch.Types

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur.
--  This function is fault-tolerant, i.e. skips over any unreadable
--  scripts.
comics :: String -> ErrorIO (M.Map T.Text LinearCrawler)
comics dir = do contents <- fErr $ getDirectoryContents dir
                (files,errs) <- filterErr (fErr . doesFileExist . (dir `combine`)) contents
                mapM_ printError errs
                res <- mapErr tryRead files
                mapM_ (mapM_ printError) (lefts res)
                let res' = map (comicName &&& id) $ rights res
                return $ M.fromList res'
   where
      fErr = catchIO "File" FileError
      tryRead :: String -> ErrorIO LinearCrawler
      tryRead fp = do c <- fErr $ readFile $ dir `combine` fp

                      case readMaybe c of Nothing -> addError $ NetworkError fp $ FileError "Couldn't parse file!"
                                          Just v  -> return v

