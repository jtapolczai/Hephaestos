{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

-- |Crawlers for linear webcomics.
module Crawling.Hephaestos.Crawlers.Linear.Load where

import Control.Arrow
import Control.Monad
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void
import System.Directory
import System.FilePath.Generic ((</>))
import Text.Read (readMaybe)

import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Types

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur. This function is fault-tolerant,
--  i.e. skips over any unreadable scripts. The read mechanism is based on Haskell's read-instances.
comics :: T.Text -- ^The directory of the scripts.
          -> ErrorIO (M.Map T.Text (SimpleLinearCrawler Void (Maybe Int))) -- ^A map of comic names and associated comics,
                                                  -- read from scripts directory.
comics dir = do contents <- liftM (map T.pack) (fErr $ getDirectoryContents (T.unpack dir))
                (files,errs) <- filterErr (fErr . doesFileExist . T.unpack . (dir </>)) contents
                mapM_ printError errs
                res <- mapErr tryRead files
                mapM_ (mapM_ printError) (lefts res)
                let res' = map (crawlerName &&& id) $ rights res
                return $ M.fromList res'
   where
      fErr = catchIO "File" FileError
      tryRead :: T.Text -> ErrorIO (SimpleLinearCrawler Void (Maybe Int))
      tryRead fp = do c <- fErr $ readFile $ T.unpack $ dir </> fp

                      case readMaybe c of
                        Nothing -> addError $ NetworkError fp $ FileError "Couldn't parse file!"
                        Just v  -> return v

