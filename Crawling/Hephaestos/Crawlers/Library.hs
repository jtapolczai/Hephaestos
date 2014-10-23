{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

-- |Crawlers for linear webcomics.
module Crawling.Hephaestos.Crawlers.Library where

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Dynamic
import Data.Either
import Data.Functor.Monadic
import Data.HList.HList
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void
import System.Directory
import System.FilePath.Generic ((</>))
import System.REPL
import Text.Read (readMaybe)

import Crawling.Hephaestos.Crawlers
import qualified Crawling.Hephaestos.Crawlers.Templates as T
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor

-- |A crawler with its configuration and state types hidden.
type PackedCrawler m = HList FetchTreeArgs
                       -> m (MTree m (SuccessorNode SomeException Dynamic))

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur. This function is fault-tolerant,
--  i.e. skips over any unreadable scripts. The read mechanism is based on Haskell's read-instances.
linearCrawlers :: T.Text -- ^The directory of the scripts.
               -> ErrorIO (M.Map T.Text (SimpleLinearCrawler m CrawlerDirection (Maybe Int)))
                 -- ^A map of comic names and associated comics,
                 -- read from scripts directory.
linearCrawlers dir =
   do contents <- liftM (map T.pack) (fErr $ getDirectoryContents (T.unpack dir))
      (files,errs) <- filterErr (fErr . doesFileExist . T.unpack . (dir </>)) contents
      mapM_ printError errs
      res <- mapErr tryRead files
      mapM_ printError $ lefts res
      return $ M.fromList $ map (crawlerName &&& id) $ rights res
   where
      fErr = catchIO "File" FileError
      tryRead :: T.Text -> ErrorIO (SimpleLinearCrawler m CrawlerDirection (Maybe Int))
      tryRead fp = do c <- fErr $ readFile $ T.unpack $ dir </> fp

                      case readMaybe c of
                        Nothing -> throwError $ SomeException $ NetworkError fp $ FileError "Couldn't parse file!"
                        Just v  -> return v

-- |Returns the list of tree crawlers.
--  Since tree crawlers can't be serialized, this is a constant.
--  Since tree crawlers are generally heterogeneous (in the types of their
--  configuration and state), they are returned in packed form, as functions
--  that internally ask for configuration data and the inital state, and
--  which return 'Dynamic' values.
treeCrawlers :: M.Map T.Text (PackedCrawler ErrorIO')
treeCrawlers = M.fromList [
  ("fileList" :: T.Text, packCrawler fileList packableFetchTree),
  ("file", packCrawler singleFile packableFetchTree)]

   where
      -- the "gallery" crawler that downloads a list of numbered files
      fileList = configCrawler "fileList" "http://*" T.fileList'
                 (ask' $ asker "Enter number of items: "
                               ("Expected positive integer!" :: T.Text)
                               "Expected positive integer!"
                               (return . (>0)))

      -- downloads a single file
      singleFile = voidCrawler "file" "http://*" T.singleFile

-- |Returns the union of 'linearCrawlers' and 'treeCrawlers'.
allCrawlers :: T.Text -> ErrorIO (M.Map T.Text (PackedCrawler ErrorIO'))
allCrawlers = linearCrawlers
              >=$> M.map (`packCrawler` packableFetchTree)
              >=$> M.union treeCrawlers
