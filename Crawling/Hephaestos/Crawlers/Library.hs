{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |Crawlers for linear webcomics.
module Crawling.Hephaestos.Crawlers.Library where

import Prelude hiding (concat, FilePath)

import Control.Concurrent.STM
import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch
import qualified Data.Collections as Co
import Data.Aeson (decode, ToJSON(toJSON))
import qualified Data.ByteString.Lazy as BL
import Data.Dynamic
import Data.Either
import Data.Functor.Monadic
import Data.ListLike (ListLike(append, snoc, concat))
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Filesystem.Path.CurrentOS' ((</>), FilePath, decodeString, encodeString, toText')
import qualified Network.URI as N
import System.Directory
import qualified System.Log.Logger as Log
import System.REPL
import System.REPL.Command
import Text.Read (readMaybe)

import qualified Crawling.Hephaestos.CLI.Format as C
import Crawling.Hephaestos.CLI.Errors (errorMsg, printError)
import Crawling.Hephaestos.Crawlers
import qualified Crawling.Hephaestos.Crawlers.Templates as T
import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Forest
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor
import Crawling.Hephaestos.I18N
import Crawling.Hephaestos.Transform

debugM x = Log.debugM ("Hephaestos.Crawlers.Library." ++ x)

type ResultSet i c v = FetchOptions -> STM () -> Command IO (ForestResult i c v)

-- |The class of showable types which can be converted to JSON.
data Ident = forall a. (Show a, ToJSON a) => Ident a

instance Show Ident where show (Ident i) = show i
instance ToJSON Ident where toJSON (Ident i) = toJSON i

-- |Case-insensitive and whitespace-removing 'elem'.
elem' :: T.Text -> [T.Text] -> Bool
elem' t ts = clean t `elem` map clean ts
   where clean = T.strip . T.toLower

-- Commonly used askers.
numAsker :: (Read a, Integral a, Functor m, Monad m) => Lang -> Asker m a
numAsker l = asker (msgs l MsgEnterNumItems)
                   (msg l MsgExpectedPosNum)
                   (msg l MsgExpectedPosNum)
                   (return . (>0))

urlAsker :: (Functor m, Monad m) => Lang -> Asker m N.URI
urlAsker l = typeAskerP (msgs l MsgEnterURL)
                        (\x -> err x
                               . N.parseURI
                               . T.unpack
                               . T.strip $ x)
   where
      err x y = maybe (Left $ msg l $ MsgInvalidUrlErr x) Right y

-- |Asks for a post-processing function to run.
--  First, the available functions are listed. Then the user is asked to select
--  one by index (0 to n), with the given argument being displayed as the
--  default.
transformAsker :: (Functor m, Monad m)
               => Lang
               -> Maybe TransformationName
                  -- ^The default transformation
               -> Asker m (Maybe TransformationName)
transformAsker l tr = case tr of
   Just _ -> maybeAskerP pr undefined parse (return . const True)
   Nothing -> typeAskerP pr (parse >=$> Just)
   where
      pr = msg l MsgTransformChoice `append` "\n"
           `append` msgs l MsgTransformOptions
           `append` concat ts
           `append` "> "

      -- number of spaces for the lines 2..n (the + 1 is because a space is inserted
      -- by msgs).
      padding = T.length (msg l MsgTransformOptions) + 1

      ts = case zip [0..] [mi..ma] of
              [] -> []
              (h:t) -> map (`snoc` '\n') $ (mkElem 0 h :) $ map (mkElem padding) t

      -- | Turn n (x,y) into "x - y", preceded by n spaces.
      --   If x == fromEnum tr, then "(default)" is added to that line
      mkElem n (x,y) = T.replicate n " " `append` T.pack (show x) `append` " - "
                       `append` (if isDef x then msgs l MsgTransformDefault else "")
                       `append` pPrint y


      pPrint NameByURL = msg l MsgNameByURL
      pPrint StructureByURL = msg l MsgStructureByURL
      pPrint StructureByKey = msg l MsgStructureByKey
      pPrint TransID = msg l MsgTransID

      isDef x = maybe False ((==) x . fromEnum) tr

      errMsg = msg l $ MsgTransformRangeErr (fromEnum mi) (fromEnum ma)

      parse = maybe (Left errMsg) toTr . readMaybe . T.unpack

      toTr x | (x >= fromEnum mi) && (x <= fromEnum ma) = Right $ toEnum x
             | otherwise                                = Left errMsg

      mi = minBound :: TransformationName
      ma = maxBound :: TransformationName

-- |Loads the list of comics from a given directory,
--  printing out any errors that occur. This function is fault-tolerant,
--  i.e. skips over any unreadable scripts. The read mechanism is based on Haskell's read-instances.
linearCrawlers :: Lang
               -> FilePath -- ^The directory of the scripts.
               -> IO [SimpleLinearCrawler]
linearCrawlers l dir =
   do createDirectoryIfMissing True dir'
      contents <- getDirectoryContents dir' >$> map decodeString
      (files,errs :: [IOException]) <- filterErr (doesFileExist . encodeString . (dir </>)) contents
      C.cliAction $ mapM_ (errorMsg l >=> printError) errs
      res <- mapErr tryRead files
      C.cliAction $ mapM_ (errorMsg l >=> printError) (lefts res :: [IOException])
      return $ rights res
   where
      dir' = encodeString dir

      tryRead :: FilePath -> IO SimpleLinearCrawler
      tryRead fp = do
         x <- BL.readFile $ encodeString $ dir </> fp
         maybe (throwM $ MetadataParsingError (toText' fp))
               return
               (decode x)

mkDyn :: (Show i, ToJSON i, Typeable a) => Successor e i a -> Successor e Ident Dynamic
mkDyn f url bs st = (fmap' <$<) <$< f url bs (fromDyn st $ error "Can't cast from dynamic!")
   where
      fmap' (SuccessorNode s r) = SuccessorNode (toDyn s) (fmap Ident r)

-- |Takes a Successor function and wraps turns it into a crawler that performs
--  post-processing and prints errors.
--simpleCrawler :: (Collection c (Path URL, SuccessorNode' Dynamic)) => FetchOptions -> URL -> TransformationName -> Successor SomeException a -> ErrorIO (ForestResult c Dynamic)
simpleCrawler l started opts uri transNum f = do
   atomically $ started
   res <- complexDownload opts (mkDyn f) undefined uri
   let trans = getTransformation transNum (opts ^. escapingFunction)
   err <- res^.metadataFiles |> mapM (trans $ res^.downloadFolder)
   C.cliAction $ mapM_ (C.error . putErrLn <=< errorMsg l) $ concat err
   return res

--TODO: store the results in a TVAR..................

-- |Returns the list of tree crawlers.
--  Since tree crawlers can't be serialized, this is a constant.
--  Since tree crawlers are generally heterogeneous (in the types of their
--  configuration and state), they are returned in packed form, as functions
--  that internally ask for configuration data and the inital state, and
--  which return 'Dynamic' values.
treeCrawlers :: (Collection coll (ResultSet Ident c Dynamic),
                 Collection c (Path N.URI, SuccessorNode' Ident Dynamic))
             => Lang
             -> coll (ResultSet Ident c Dynamic)
treeCrawlers l = Co.insertMany [fileList,
                                file,
                                images,
                                fileTypes,
                                xPath] Co.empty

   where
      trAsker = transformAsker l $ Just NameByURL
      urlAsker' = urlAsker l

      fileList :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      fileList opts s =
         makeCommand3 name (`elem'` [name]) desc trAsker urlAsker' (numAsker l) crawler
         where
            name = "fileList"
            desc = msg l MsgFileListCrawlerDesc
            crawler _ transNum url num =
               simpleCrawler l s opts url (fromMaybe NameByURL transNum) (T.fileList' num)

      file :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      file opts s =
         makeCommand2 name (`elem'` [name]) desc trAsker urlAsker' crawler
         where
            name = "file"
            desc = msg l MsgFileCrawlerDesc
            crawler _ transNum url =
               simpleCrawler l s opts url (fromMaybe NameByURL transNum) T.singleFile

      images :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      images opts s =
         makeCommand2 name (`elem'` [name]) desc trAsker urlAsker' crawler
         where
            name = "images"
            desc = msg l MsgImagesCrawlerDesc
            crawler _ transNum url =
               simpleCrawler l s opts url (fromMaybe NameByURL transNum) T.allImages

      fileTypes :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      fileTypes opts s =
         makeCommand4 name (`elem'` [name]) desc trAsker urlAsker' tagAsker extAsker crawler
         where
            name = "fileTypes"
            desc = msg l MsgFileTypesCrawlerDesc

            tagAsker = typeAsker (msgs l $ MsgFileTypesCrawlerTag
                                          "[(\"img\", \"src\"), (\"a\", \"href\")]")
                                  (msg l MsgFileTypesCrawlerTagErr)

            extAsker = typeAsker (msgs l $ MsgFileTypesCrawlerExt
                                          "[\".jpg\", \".png\"]")
                                 (msg l MsgFileTypesCrawlerExtErr)

            crawler _ transNum url tags exts =
               simpleCrawler l s opts url (fromMaybe NameByURL transNum)
                             (T.allElementsWhereExtension tags exts)

      xPath :: Collection c (Path N.URI, SuccessorNode' Ident Dynamic) => ResultSet Ident c Dynamic
      xPath opts s =
         makeCommand3 name (`elem'` [name]) desc trAsker urlAsker' xPathAsker crawler
         where
            name = "xPath"
            desc = msg l MsgXPathCrawlerDesc

            xPathAsker = predAsker (msgs l MsgXPathCrawlerPath)
                                   (msg l MsgExpectedNonEmpty)
                                   (return  . not . T.null . T.strip)

            crawler _ transNum url (Verbatim xp) =
               simpleCrawler l s opts url (fromMaybe NameByURL transNum)
                             (T.xPathCrawler xp)


-- |Returns the union of 'linearCrawlers' and 'treeCrawlers'.
allCrawlers :: (Collection coll (ResultSet Ident c Dynamic),
                Collection c (Path N.URI, SuccessorNode' Ident Dynamic))
            => Lang
            -> FilePath
            -> IO (coll (ResultSet Ident c Dynamic))
allCrawlers l = linearCrawlers l
                >=$> map (packCrawlerL l)
                >=$> flip Co.insertMany (treeCrawlers l)

-- |Packs a SimpleLinearCrawler crawler into a runnable format, "runnable"
--  meaning that it can be executed as an IO action which will get its
--  arguments from stdin.
packCrawlerL :: (Collection c (Path N.URI, SuccessorNode' Ident Dynamic))
             => Lang
             -> SimpleLinearCrawler
             -> ResultSet Ident c Dynamic
packCrawlerL l cr opts started =
   makeCommand4 (slcName cr)
                (`elem'` [slcName cr])
                (slcDescription cr)
                (transformAsker l $ Just NameByURL)
                dirAsker
                urlAsker'
                numAsker'
                (\_ transNum dir' url' num ->
                     let
                        trans = getTransformation (fromMaybe NameByURL transNum)
                                                  (opts ^. escapingFunction)
                        dir = fromMaybe Forwards dir'
                        f = mkDyn $ crawlerNext cr dir
                        url = fromMaybe (case dir of Forwards -> slcFirstURL cr
                                                     Backwards -> slcLastURL cr)
                                        url'
                     in
                        do atomically started
                           res <- complexDownload opts f (toDyn num) url
                           debugM "packCrawlerL" "complexDownload finished."
                           let mfs = res ^. metadataFiles
                           err <- mfs |> mapM (trans $ res^.downloadFolder)
                           debugM "packCrawlerL" "trans finished."
                           mapM_ (C.error . putErrLn <=< errorMsg l) $ concat err
                           debugM "packCrawlerL" "mapM_ finished."
                           return res)

   where
      dirAsker = maybeAskerP (msgs l MsgDirAsker)
                             undefined
                             fbParse
                             (const $ return True)

      urlAsker' = maybeAskerP (msgs l MsgEnterURLMaybe)
                              undefined
                              (\x -> err x
                                     . N.parseURI
                                     . T.unpack
                                     . T.strip $ x)
                              (const $ return True)

      err x y = maybe (Left $ msg l $ MsgInvalidUrlErr x) Right y

      fbParse t | T.strip t == "F" = Right Forwards
                | T.strip t == "B" = Right Backwards
                | otherwise        = Left $ msg l MsgExpectedDir

      numAsker' :: (Functor m, Monad m) => Asker m (Maybe Int)
      numAsker' = maybeAsker (msgs l MsgMaxNumOfPages)
                             (msg l MsgExpectedPosNum)
                             (msg l MsgExpectedPosNum)
                             (return . (>= 0))
