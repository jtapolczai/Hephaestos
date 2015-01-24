{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Crawlers for linear webcomics.
module Crawling.Hephaestos.Crawlers.Library where

import Prelude hiding (concat, append, FilePath)

import Control.Arrow
import Control.Exception
import Control.Lens (makeLenses, (&), (%~), (^.))
import Control.Monad
import Control.Monad.Catch
import qualified Data.Collections as Co
import qualified Data.Collections.BulkInsertable as Co
import Data.Aeson (decode, ToJSON(toJSON))
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Dynamic
import Data.Either
import Data.Functor.Monadic
import Data.ListLike (ListLike(append, snoc, concat))
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text.Lazy as T
import Data.Tree.Monadic (Path)
import Data.Types.Injective
import Data.Void
import Filesystem.Path.CurrentOS' ((</>), FilePath, decodeString, encodeString, toText')
import Network.HTTP.Conduit (Request)
import qualified Network.URI as N
import System.Directory
import System.REPL
import System.REPL.Command
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Text.Read (readMaybe)

import qualified Crawling.Hephaestos.CLI.Color as C

import Crawling.Hephaestos.Crawlers
import qualified Crawling.Hephaestos.Crawlers.Templates as T
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Forest
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor
import Crawling.Hephaestos.Transform

type ResultSet i c v = FetchOptions -> Command IO (ForestResult i c v)

-- |The class of showable types which can be converted to JSON.
data Ident = forall a. (Show a, ToJSON a) => Ident a

instance Show Ident where show (Ident i) = show i
instance ToJSON Ident where toJSON (Ident i) = toJSON i

-- |Case-insensitive and whitespace-removing 'elem'.
elem' :: T.Text -> [T.Text] -> Bool
elem' t ts = clean t `elem` map clean ts
   where clean = T.strip . T.toLower

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

-- |Asks for a post-processing function to run.
--  First, the available functions are listed. Then the user is asked to select
--  one by index (0 to n), with the given argument being displayed as the
--  default.
transformAsker :: (Functor m, Monad m) => TransformationName -> Asker m (Maybe TransformationName)
transformAsker tr = maybeAskerP pr undefined parse (return . const True)
   where
      pr = "What should be done to the output?\n"
           `append` "Options: " `append` concat ts `append` "> "

      ts = case zip [0..] [mi..ma] of
              [] -> []
              (h:t) -> map (`snoc` '\n') $ (mkElem 0 h :) $ map (mkElem 9) t

      -- | Turn n (x,y) into "x - y", preceded by n spaces.
      --   If x == fromEnum tr, then "(default)" is added to that line
      mkElem n (x,y) = T.pack $ replicate n ' ' `append` show x `append` " - "
                       `append` (if x == fromEnum tr then "(default) " else "")
                       `append` prettyShow y

      errMsg = to $ "Expected a number between " `append` show (fromEnum mi)
                    `append` " and " `append` show (fromEnum ma) `append` "!"

      parse = maybe (Left errMsg) toTr . readMaybe . T.unpack

      toTr x | (x >= fromEnum mi) && (x <= fromEnum ma) = Right $ toEnum x
             | otherwise                                = Left errMsg

      mi = minBound :: TransformationName
      ma = maxBound :: TransformationName

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur. This function is fault-tolerant,
--  i.e. skips over any unreadable scripts. The read mechanism is based on Haskell's read-instances.
linearCrawlers :: FilePath -- ^The directory of the scripts.
               -> IO [SimpleLinearCrawler]
linearCrawlers dir =
   do createDirectoryIfMissing True dir'
      contents <- getDirectoryContents dir' >$> map decodeString
      (files,errs :: [IOException]) <- filterErr (doesFileExist . encodeString . (dir </>)) contents
      mapM_ printError errs
      res <- mapErr tryRead files
      mapM_ printError $ (lefts res :: [IOException])
      return $ rights res
   where
      dir' = encodeString dir

      tryRead :: FilePath -> IO SimpleLinearCrawler
      tryRead fp = do
         x <- BL.readFile $ encodeString $ dir </> fp
         maybe (throwM $ dataFormatError (toText' fp) "Couldn't parse file!")
               return
               (decode x)

mkDyn :: (Show i, ToJSON i, Typeable a) => Successor e i a -> Successor e Ident Dynamic
mkDyn f url bs st = fmap fmap' $ f url bs (fromDyn st $ error "Can't cast from dynamic!")
   where
      fmap' (SuccessorNode s r) = SuccessorNode (toDyn s) (fmap Ident r)

-- |Takes a Successor function and wraps turns it into a crawler that performs
--  post-processing and prints errors.
--simpleCrawler :: (Collection c (Path URL, SuccessorNode' Dynamic)) => FetchOptions -> URL -> TransformationName -> Successor SomeException a -> ErrorIO (ForestResult c Dynamic)
simpleCrawler opts url transNum f = do
   case N.parseURIReference $ T.unpack url of
      Nothing -> throwM $ dataFormatError url noParse
      Just uri -> do
         res <- complexDownload opts (mkDyn f) undefined uri
         let trans = getTransformation transNum
         err <- res^.metadataFiles |> mapM (trans $ res^.downloadFolder)
         mapM_ (C.error . putErrLn . show) $ concat err
         return res

   where
      noParse = "Couldn't parse '" `append` url `append` "' as URL!"

-- |Returns the list of tree crawlers.
--  Since tree crawlers can't be serialized, this is a constant.
--  Since tree crawlers are generally heterogeneous (in the types of their
--  configuration and state), they are returned in packed form, as functions
--  that internally ask for configuration data and the inital state, and
--  which return 'Dynamic' values.
treeCrawlers :: (Collection coll (ResultSet Ident c Dynamic),
                 Collection c (Path N.URI, SuccessorNode' Ident Dynamic))
             => coll (ResultSet Ident c Dynamic)
treeCrawlers = flip Co.insertMany Co.empty [fileList,
                                            file,
                                            images,
                                            fileTypes,
                                            xPath]

   where
      fileList :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      fileList opts =
         makeCommand3 name (`elem'` [name]) desc (transformAsker NameByURL) urlAsker numAsker crawler
         where
            name = "fileList"
            desc = "downloads a list of numbered files"
            crawler _ transNum (Verbatim url) num =
               simpleCrawler opts url (fromMaybe NameByURL transNum) (T.fileList' num)

      file :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      file opts =
         makeCommand2 name (`elem'` [name]) desc (transformAsker NameByURL) urlAsker crawler
         where
            name = "file"
            desc = "downloads a single file."
            crawler _ transNum (Verbatim url) =
               simpleCrawler opts url (fromMaybe NameByURL transNum) T.singleFile

      images :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      images opts =
         makeCommand2 name (`elem'` [name]) desc (transformAsker NameByURL) urlAsker crawler
         where
            name = "images"
            desc = "downloads all (linked) images from a page."
            crawler _ transNum (Verbatim url) =
               simpleCrawler opts url (fromMaybe NameByURL transNum) T.allImages

      fileTypes :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      fileTypes opts =
         makeCommand4 name (`elem'` [name]) desc (transformAsker NameByURL) urlAsker tagAsker extAsker crawler
         where
            name = "fileTypes"
            desc = "downloads all files of a given type."

            tagAsker = typeAsker "Enter list allowed tags/attributes\n\
                                  \e.g. [(\"img\", \"src\"), (\"a\", \"href\")]: "
                                  "Expected list of pairs!"

            extAsker = typeAsker "Enter list of allowed file extensions\n\
                                  \e.g. [\".jpg\", \".png\"]: "
                                 "Expected list of file extensions!"

            crawler _ transNum (Verbatim url) tags exts =
               simpleCrawler opts url (fromMaybe NameByURL transNum)
                             (T.allElementsWhereExtension tags exts)

      xPath :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      xPath opts =
         makeCommand3 name (`elem'` [name]) desc (transformAsker NameByURL) urlAsker xPathAsker crawler
         where
            name = "xPath"
            desc = "Gets all URLs returned by an XPath-expression."

            xPathAsker = predAsker "Enter XPath-expression: "
                                   "Expected non-empty string!"
                                   (return  . not . T.null . T.strip)

            crawler _ transNum (Verbatim url) (Verbatim xp) =
               simpleCrawler opts url (fromMaybe NameByURL transNum)
                             (T.xPathCrawler xp)



-- |Returns the union of 'linearCrawlers' and 'treeCrawlers'.
allCrawlers :: (Collection coll (ResultSet Ident c Dynamic),
                Collection c (Path N.URI, SuccessorNode' Ident Dynamic))
            => FilePath
            -> IO (coll (ResultSet Ident c Dynamic))
allCrawlers = linearCrawlers
              >=$> map packCrawlerL
              >=$> flip Co.insertMany treeCrawlers
   where

packCrawlerL :: (Collection c (Path N.URI, SuccessorNode' Ident Dynamic))
             => SimpleLinearCrawler
             -> ResultSet Ident c Dynamic
packCrawlerL cr opts =
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
                        complexDownload opts f (toDyn num) url)
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
