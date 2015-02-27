{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Basic downloading and file saving functions.
module Crawling.Hephaestos.Fetch (
   -- *Task categories
   -- |These categories are used by 'download' to send updates about running
   --  downloads. For full book-keeping, the 'downloadStatus' of 'FetchOptions'
   --  that are passed to 'download' must have all of the below categories
   --  (they don't /have/ to be present, however).
   downloadingTasks,
   finishedTasks,
   failedTasks,
   -- * Downloading
   simpleDownload,
   download,
   downloadWhole,
   consume,
   saveFile,
   downloadsFolder,

   module Crawling.Hephaestos.Fetch.ErrorHandling,
   )where

import Prelude hiding (concat, reverse, takeWhile, (++), putStrLn, writeFile, FilePath)

import Control.Concurrent.STM
import Control.Concurrent.STM.Utils
import Control.Exception (SomeException(..))
import Control.Lens ((&), (%~), (^.), (+~), (.~))
import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS (readInteger)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as Con
import qualified Data.Conduit.Binary as ConB
import qualified Data.Conduit.List as ConL
import Data.Default
import Data.Functor
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.Maybe (isJust)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Filesystem.Path.CurrentOS' hiding (append, encode)
import Network.HTTP.Conduit hiding (path, withManager)
import Network.HTTP.Types.Header (hContentLength)
import Network.Socket.Internal
import Network.URI (URI)
import System.Directory
import qualified System.Log.Logger as Log

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor hiding (reqMod)
import Crawling.Hephaestos.Fetch.ErrorHandling

infoM x = liftIO . Log.infoM ("Hephaestos.Fetch." L.++ x)
debugM x = liftIO . Log.debugM ("Hephaestos.Fetch." L.++ x)
alertM x = liftIO . Log.alertM ("Hephaestos.Fetch." L.++ x)

-- |Category idenfitier for currently running downloads.
downloadingTasks :: TaskCat
downloadingTasks = TaskCat 0

-- |Category identifier for successfully finished downloads.
finishedTasks :: TaskCat
finishedTasks = TaskCat 1

-- |Category identifier for failed downloads.
failedTasks :: TaskCat
failedTasks = TaskCat 2

-- |Gets the content of an URL.
--  @simpleDownload = withSocketsDo . simpleHttp@ and thus, caveats of
--  @simpleHttp@ apply.
simpleDownload :: URL -> IO BL.ByteString
simpleDownload = withSocketsDo . simpleHttp . T.unpack

-- |Downloads a whole resource and returns the contents as soon as the
--  downloading process has finished.
--
--  Calling this function is perfectly fine, but keep in mind that
--
--  1. the whole resource will be downloaded before it returns and
--  2. all of it will be kept in memory.
--
--  For large files and more fine-grained control, use 'download'.
downloadWhole :: FetchOptions -> URI -> IO BL.ByteString
downloadWhole opts url = runResourceT $ do
   (_,content) <- download opts url
   consume content

-- |Consumes an entire conduit and returns the contents.
consume :: Con.Source (ResourceT IO) BL.ByteString -> ResourceT IO BL.ByteString
consume content = content Con.$$ (ConL.map BL.toStrict Con.=$= ConL.consume)
                  >$> BL.fromChunks

-- |Downloads the contents of a URL and periodically provides information
--  about the download's progress via the 'downloadStatus' field of the
--  'FetchOptions' argument.
--
--  == Laziness
--  This function always sends a request, but it returns as soon as the response
--  headers arrive. The response body can be accessed via the returned conduit,
--  meaning that large responses can be streamed on demand.
--
--  == Download status
--  When a download is starts, it is placed in the
--  'downloadingTasks' category, where it will be continuously updated as data
--  comes in. If a download fails, it is put in the 'failedTasks' category;
--  if it finishes successfully, it is put into 'finishedTasks'. In either
--  of these two cases, it is not touched again.
--
--  In principle, any other function is free to modify the TVar as it sees fit
--  (i.e. doing so will not cause exceptions), but modifying the data of
--  currently running downloads is not really sensible.
--
--  /Note/: if you do not want download statuses to clog up memory, simply pass
--          a 'FetchOptions' object that lacks any or all of
--          @{downloadingsTasks, finishedTasks, failedTasks}@.
download :: FetchOptions
            -- ^Array for storing the download progress.
            --  'download' will find the lowest unused key in the range [0..]
            --  and insert a new 'Download' item under it, which it will
            --  modify as the resource is downloaded.
         -> URI -- ^The URL.
         -> ResourceT IO (Int, Con.Source (ResourceT IO) BL.ByteString)
            -- ^The key for the current download along with the 'Conduit' from
            --  which the contents can be fetched.
download opts url = do
   req <- (opts ^. reqFunc) <$> parseUrl (show url)
   key <- atomically' $ insertTask sl downloadingTasks
                                  (def & downloadURL .~ T.pack (show url))
   infoM "download" $ "Sending request to " L.++ show url L.++ "."
   -- send the request
   res <- http req (opts ^. manager)
   -- first, we unwrap the source, turning the ResumableSource into a
   -- regular one. This is done because ResumableSource doesn't have a bracketP.
   debugM "download" $ "Response arrived from " L.++ show url L.++ "."
   (src, finalizer) <- Con.unwrapResumable $ responseBody res
   let -- we need to do four things in addition to just streaming the data:
       -- 1. continuously update the number of downloaded bytes
       conduit = src Con.$= reportProgressConduit key
       -- 2. set the length (if available) and the download status when the
       --    download starts
       whenBegin = update key (\s -> s & downloadSize .~ clength res
                                       & downloadStatus .~ InProgress)
       -- 3. set the download status to 'Failed' in case of errors.
       whenErr (e :: SomeException) = do
         atomically' (do update key (& downloadStatus .~ Failed (SomeException e))
                         transferTask sl downloadingTasks key failedTasks)
         throwM e
       -- 4. set the download status to 'Finished' in the end.
       whenEnd = do atomically (do update key (& downloadStatus .~ Finished)
                                   transferTask sl downloadingTasks key finishedTasks)
                    infoM "download" $ "Download from " L.++ show url L.++ " finished."
       -- for some reason, bracketP is specialized to IO. We therefore have to
       -- add the finalizer via addCleanup.
       conduit' = Con.addCleanup (const finalizer)
                  $ Con.bracketP (atomically whenBegin)
                                 (const whenEnd)
                                 (const $ Con.handleC whenErr conduit)
       -- NOTE: all of this ONLY serves to update the TVar and thereby inform
       -- anyone listening of the download progress. 'download' and thge conduit
       -- it returns make no attempt at dealing with exceptions. That is left
       -- to higher-level functions.
   return (key, conduit')
   where
      sl = opts ^. downloadCategories
      update = updateTask sl downloadingTasks

      reportProgressConduit :: Int -> Con.Conduit BS.ByteString (ResourceT IO) BL.ByteString
      reportProgressConduit slot = do
         open <- ConL.peek >$> isJust
         when open $ do
            chunk <- ConB.take 8192
            atomically' $ update slot (& downloadBytes +~ fromIntegral (BL.length chunk))
            Con.yield chunk
            reportProgressConduit slot

      clength :: Response a -> Maybe Integer
      clength r = lookup hContentLength (responseHeaders r)
                  >>= BS.readInteger
                  >$> fst

-- |Saves the contents of a HTTP response to a local file.
--  If the save directory does not exist, it will be created.
--
--  This function doesn't catch any exceptions and doesn't wrap exceptions
--  into failure nodes.
--
--  If the result type is a failure and the 'maxFailureNodes' setting
--  is 0, the file won't be saved at all.
--
--  This function is partial; inner nodes will not be saved
--  (except when they are wrapped in failure-nodes).
saveFile :: forall e i.Show e
         => FetchOptions
         -> FilePath -- ^The root of the filename under which to save. Should not contain the extension.
         -> FetchResult e i -- ^Contents of the file
         -> IO (Maybe FilePath) -- ^The actual filename under which the response was saved.
saveFile opts fn response
   | isFailure response && (opts ^. maxFailureNodes) <<= 0 = return Nothing
   | otherwise = do
      let path = opts ^. savePath
      createDirectoryIfMissing True (encodeString path)

      -- get a conduit and slam everything into the target file
      let fn' = encodeString $ path </> fn <.> ext response
      runResourceT $ do
         content <- action response
         -- we first fuse "toStrict" and "sinkFile" and give that as the finaliser
         -- for "content".
         content Con.$$ toStrict Con.=$= ConB.sinkFile fn'

      return $ Just $ fn <.> ext response
   where
      toStrict = Con.awaitForever $ Con.yield . BL.toStrict

      -- downloads a Blob and gets the contents
      action :: FetchResult e i -> ResourceT IO (Con.Source (ResourceT IO) BL.ByteString)
      action (Blob _ url reqMod) = withTaskLimit (opts ^. taskLimit) $
         download (opts & reqFunc %~ (reqMod.)) url >$> snd

      --gets a ByteString out of a non-Blob leaf
      action (PlainText _ p) = return' $ T.encodeUtf8 p
      action (XmlResult _ p) = return' $ B.encode p
      action (BinaryData _ p) = return' p
      action (Info _ k v) = return' $ encode $ object ["key" .= k, "value" .= v]
      action f@(Failure _ _) = return' $ encode $ action' f (opts ^. maxFailureNodes) 0
      action Inner{} = liftIO (alertM "saveFile" "called saveFile with Inner node!")
                       >>       error            "called saveFile with Inner node!"

      -- saves a chain of failure nodes, omitting some if the maxFailureNodes
      -- limit is reached.
      -- we have two "modes": the regular one, in which we create a nested
      -- object, and the "limit reached" one, in which we go straight to
      -- the root and count the number of omitted nodes along the way.
      action' :: Show e => FetchResult e i -> Maybe Int -> Int -> Value
      action' (Failure _ (Just (orig, _))) (Just 0) omitted = action' orig (Just 0) $ omitted + 1
      action' f@(Failure e (Just (orig, name))) limit omitted =
         object' ["error" .= show e, "type" .= ext f,
                  "child" .= action' orig (limit >$> subtract 1) omitted]
                  [("filename", name >$> encodeString)]

      -- the root of a failure chain
      action' f _ n = object' ["type" .= ext f]
                              [("url", getURL f >$> show >$> toJSON),
                               ("omitted", n' >$> toJSON),
                               ("error", getError f >$> show >$> toJSON)]
         where
            n' = if n == 0 then Nothing else Just n

-- |Gets the user's Downloads folder. This is assumed to be
--  the directory named \"Dowloads\" (case sensitive)
--  in the user's home directory.
downloadsFolder :: (MonadIO m, Functor m) => m FilePath
downloadsFolder = liftIO getHomeDirectory
                  >$> decodeString
                  >$> (</> decodeString "Downloads")
                  >$> encodeString
                  >>= liftIO . canonicalizePath
                  >$> decodeString


-- Helpers
-------------------------------------------------------------------------------

(<<=) :: Ord a => Maybe a -> a -> Bool
(<<=) Nothing _ = False
(<<=) (Just x) y = x <= y

-- |Creates a new resumable source from a value.
return' :: Monad m => o -> ResourceT IO (Con.Source m o)
return' = return . Con.yield

-- creates a JSON object out of a list of mandatory and a list of
-- optional fields
object' xs = object . (xs L.++) . map (\(k,Just v) -> k.=v) . filter (isJust.snd)
