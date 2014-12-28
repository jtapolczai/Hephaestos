{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |Crawlers for linear webcomics.
module Crawling.Hephaestos.Crawlers.Library where

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Except
import qualified Data.Collections as Co
import qualified Data.Collections.BulkInsertable as Co
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Data.Dynamic
import Data.Either
import Data.Functor.Monadic
import Data.HList.HList
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T
import Data.Tree.Monadic (Path)
import Data.Types.Injective
import Data.Void
import Network.HTTP.Conduit (Request)
import System.Directory
import System.FilePath.Generic ((</>))
import System.REPL
import System.REPL.Command

import Crawling.Hephaestos.Crawlers
import qualified Crawling.Hephaestos.Crawlers.Templates as T
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Forest
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Helper.String (elem')

data Stringy = forall a. Injective a T.Text => Stringy a

type StaticArgs = [Manager, (Request -> Request), Stringy]
type ResultSet c v = HList StaticArgs -> Command ErrorIO' (ForestResult c v)

-- Commonly used askers.
numAsker :: (Read a, Integral a, Functor m, Monad m) => Asker m a
numAsker = asker "Enter number of items: "
                 ("Expected positive integer!" :: T.Text)
                 "Expected positive integer"
                 (return . (>0))

urlAsker :: (Functor m, Monad m) => Asker m Verbatim
urlAsker = predAsker "Enter URL: "
                     "Expected non-empty string!"
                     (return  . not . T.null . T.strip)

-- |A crawler with its configuration and state types hidden.
--type PackedCrawler m = HList StaticArgs
--                       -> m (MTree m (SuccessorNode SomeException Dynamic))

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur. This function is fault-tolerant,
--  i.e. skips over any unreadable scripts. The read mechanism is based on Haskell's read-instances.
linearCrawlers :: T.Text -- ^The directory of the scripts.
               -> ErrorIO [SimpleLinearCrawler]
linearCrawlers dir =
   do contents <- liftM (map T.pack) (catchIO dir FileError $ getDirectoryContents (T.unpack dir))
      (files,errs) <- filterErr (catchIO dir FileError . doesFileExist . T.unpack . (dir </>)) contents
      mapM_ printError errs
      res <- mapErr tryRead files
      mapM_ printError $ lefts res
      return $ rights res
   where
      tryRead :: T.Text -> ErrorIO SimpleLinearCrawler
      tryRead fp = do
         x <- (catchIO fp FileError $ BL.readFile $ T.unpack $ dir </> fp)
         maybe (throwError $ SomeException $ NetworkError fp $ FileError "Couldn't parse file!")
               return
               (decode x)

mkDyn :: Typeable a => Successor e a -> Successor e Dynamic
mkDyn f url bs st = fmap (fmap toDyn) $ f url bs (fromDyn st $ error "can't cast from dynamic!")

-- |Returns the list of tree crawlers.
--  Since tree crawlers can't be serialized, this is a constant.
--  Since tree crawlers are generally heterogeneous (in the types of their
--  configuration and state), they are returned in packed form, as functions
--  that internally ask for configuration data and the inital state, and
--  which return 'Dynamic' values.
treeCrawlers :: (Collection coll (ResultSet c Dynamic),
                 Collection c (Path URL, SuccessorNode' Dynamic))
             => coll (ResultSet c Dynamic)
treeCrawlers = flip Co.insertMany Co.empty [fileList,
                                            file,
                                            images,
                                            fileTypes]

   where
      fileList :: Collection c (Path URL, SuccessorNode' Dynamic) => ResultSet c Dynamic
      fileList (HCons m (HCons req (HCons (Stringy savePath) HNil))) =
         makeCommand2 name (`elem'` [name]) desc urlAsker numAsker crawler
         where
            name = "fileList"
            desc = "downloads a list of numbered files"
            crawler _ (Verbatim url) num =
               complexDownload m req (to savePath) (mkDyn $ T.fileList' num) undefined url

      file :: Collection c (Path URL, SuccessorNode' Dynamic) => ResultSet c Dynamic
      file (HCons m (HCons req (HCons (Stringy savePath) HNil))) =
         makeCommand1 name (`elem'` [name]) desc urlAsker crawler
         where
            name = "file"
            desc = "downloads a single file."
            crawler _ (Verbatim url) =
               complexDownload m req (to savePath) (mkDyn T.singleFile) undefined url


      images :: Collection c (Path URL, SuccessorNode' Dynamic) => ResultSet c Dynamic
      images (HCons m (HCons req (HCons (Stringy savePath) HNil))) =
         makeCommand1 name (`elem'` [name]) desc urlAsker crawler
         where
            name = "images"
            desc = "downloads all (linked) images from a page."
            crawler _ (Verbatim url) =
               complexDownload m req (to savePath) (mkDyn T.allImages) undefined url

      fileTypes :: Collection c (Path URL, SuccessorNode' Dynamic) => ResultSet c Dynamic
      fileTypes (HCons m (HCons req (HCons (Stringy savePath) HNil))) =
         makeCommand3 name (`elem'` [name]) desc urlAsker tagAsker extAsker crawler
         where
            name = "fileTypes"
            desc = "downloads all files of a given type."

            tagAsker = typeAsker "Enter list allowed tags/attributes\n\
                                  \e.g. [(\"img\", \"src\"), (\"a\", \"href\")]: "
                                  "Expected list of pairs!"

            extAsker = typeAsker "Enter list of allowed file extensions\n\
                                  \e.g. [\".jpg\", \".png\"]: "
                                 "Expected list of file extensions!"

            crawler _ (Verbatim url) tags exts =
               complexDownload m req (to savePath)
                               (mkDyn $ T.allElementsWhereExtension tags exts)
                               undefined
                               url

-- |Returns the union of 'linearCrawlers' and 'treeCrawlers'.
allCrawlers :: (Collection coll (ResultSet c Dynamic),
                Collection c (Path URL, SuccessorNode' Dynamic))
            => T.Text
            -> ErrorIO (coll (ResultSet c Dynamic))
allCrawlers = linearCrawlers
              >=$> map packCrawlerL
              >=$> flip Co.insertMany treeCrawlers
   where

packCrawlerL :: (Collection c (Path URL, SuccessorNode' Dynamic))
             => SimpleLinearCrawler
             -> ResultSet c Dynamic
packCrawlerL cr (HCons m (HCons req (HCons (Stringy savePath) HNil))) =
   makeCommand2 (slcName cr)
                (`elem'` [slcName cr])
                (slcDescription cr)
                dirAsker
                numAsker'
                (\_ dir' num ->
                     let
                        dir = maybe Forwards id dir'
                        f = mkDyn $ crawlerNext cr dir
                        url = case dir of Forwards -> slcFirstURL cr
                                          Backwards -> slcLastURL cr
                     in
                        complexDownload m req (to savePath) f (toDyn num) url)
   where
      dirAsker = maybeAsker "Enter direction (Forwards/Backwards; default=Forwards): "
                            "Expected Forwards/Backwards."
                            undefined
                            (const $ return True)

      numAsker' :: (Functor m, Monad m) => Asker m (Maybe Int)
      numAsker' = maybeAsker "Enter max. number of pages to get (or leave blank): "
                             "Expected positive integer!"
                             "Expected positive integer!"
                             (return . (>= 0))
