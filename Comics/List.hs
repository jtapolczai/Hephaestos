{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |Crawlers for linear webcomics.
module Comics.List where

import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import Text.Read (readMaybe)


import Comics.LinearComic
import Fetch.ErrorHandling
import Fetch.Types

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur.
--  This function is fault-tolerant, i.e. skips over any unreadable
--  scripts.
comics :: String -> ErrorIO (M.Map T.Text LinearComic)
comics dir = do contents <- fErr $ getDirectoryContents dir
                (files,errs) <- filterErr (fErr . doesFileExist) contents
                mapM_ printError errs
                res <- mapErr tryRead files
                mapM_ (mapM_ printError) (lefts res)
                let res' = map (\(Just v) -> (comicName v, v)) $ filter isJust $ rights res
                return $ M.fromList res'
   where
      fErr = catchIO "File" FileError
      tryRead fp = do c <- fErr $ readFile fp
                      return $ readMaybe c

xkcd :: LinearComic
xkcd =
   LinearComic "XKCD"
               "http://www.xkcd.com/"
               "http://www.xkcd.com/1/"
               "http://www.xkcd.com"
               "//div[@id=\"comic\"]/img/@src/text()"
               "(//a[@rel=\"next\"])[1]/@href/text()"
               "(//a[@rel=\"prev\"])[1]/@href/text()"

pennyArcade :: LinearComic
pennyArcade =
   LinearComic "Penny Arcade"
               "http://www.penny-arcade.com/"
               "http://www.penny-arcade.com/comic/1998/11/18"
               "http://www.penny-arcade.com/comic"
               "//*[@id=\"comicFrame\"]//img/@src/text()"
               "(//a[@class=\"btn btnNext\"])[1]/@href/text()"
               "(//a[@class=\"btn btnPrev\"])[1]/@href/text()"

cyanideAndHappiness :: LinearComic
cyanideAndHappiness =
   LinearComic "Cyanide and Happiness"
               "http://www.explosm.net/"
               "http://explosm.net/comics/15/"
               "http://explosm.net/comics/"
               "//*[@id=\"maincontent\"]/div[2]/div[1]/img/@src/text()"
               "//a[@rel=\"next\"]/@href/text()"
               "//a[@rel=\"prev\"]/@href/text()"
