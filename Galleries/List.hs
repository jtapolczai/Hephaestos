{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

-- |Crawlers for linear webcomics.
module Galleries.List where

import Control.Arrow
import Control.Monad
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import System.Directory
import System.FilePath.Posix.Generic ((</>))
import Text.Read (readMaybe)


import Galleries.Linear
import Fetch.ErrorHandling
import Fetch.Types

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur. This function is fault-tolerant,
--  i.e. skips over any unreadable scripts. The read mechanism is based on Haskell's read-instances.
comics :: T.Text -- ^The directory of the scripts.
          -> ErrorIO (M.Map T.Text LinearCrawler) -- ^A map of comic names and associated comics,
                                                  -- read from scripts directory.
comics dir = do contents <- liftM (map T.pack) (fErr $ getDirectoryContents (T.unpack dir))
                (files,errs) <- filterErr (fErr . doesFileExist . T.unpack . (dir </>)) contents
                mapM_ printError errs
                res <- mapErr tryRead files
                mapM_ (mapM_ printError) (lefts res)
                let res' = map (comicName &&& id) $ rights res
                return $ M.fromList res'
   where
      fErr = catchIO "File" FileError
      tryRead :: T.Text -> ErrorIO LinearCrawler
      tryRead fp = do c <- fErr $ readFile $ T.unpack $ dir </> fp

                      case readMaybe c of
                        Nothing -> addError $ NetworkError fp $ FileError "Couldn't parse file!"
                        Just v  -> return v

