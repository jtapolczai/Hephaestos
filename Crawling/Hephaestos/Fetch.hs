{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Basic downloading and file saving functions.
module Crawling.Hephaestos.Fetch (
   -- * Downloading
   simpleDownload,
   download,
   downloadWhole,
   saveFile,
   downloadsFolder,

   module Crawling.Hephaestos.Fetch.ErrorHandling,
   )where

import Prelude hiding (concat, reverse, takeWhile, (++), putStrLn, writeFile, FilePath)

import Control.Concurrent.STM
import Control.Concurrent.STM.Utils
import Control.Lens ((&), (%~), (^.), (+~), (.~))
import Control.Monad (when)
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
import qualified Data.IntMap as IM
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

import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor
import Crawling.Hephaestos.Fetch.ErrorHandling
import System.REPL

-- |Gets the content of an URL.
--  @simpleDownload = withSocketsDo . simpleHttp@ and thus, caveats of
--  @simpleHttp@ apply.
simpleDownload :: URL -> IO BL.ByteString
simpleDownload = withSocketsDo . simpleHttp . T.unpack

-- |Downloads a whole resource and returns the contents as soon as the
--  downloading process has finished.
downloadWhole :: FetchOptions -> URI -> IO BL.ByteString
downloadWhole opts url = runResourceT $ do
   (_,content) <- download opts url
   content Con.$$+- (ConL.map BL.toStrict Con.=$= ConL.consume) >$> BL.fromChunks

-- |Downloads the contents of a URL and periodically provides information
--  about the download's progress.
--
--  Note that the download task will be created as soon as the request is
--  sent, not when the response arrives. This means that request to non-
--  responsive servers will already appear.
download :: FetchOptions
            -- ^Array for storing the download progress.
            --  'download' will find the lowest unused key in the range [0..]
            --  and insert a new 'Download' item under it, which it will
            --  modify as the resource is downloaded.
         -> URI -- ^The URL.
         -> ResourceT IO (Int, Con.ResumableSource (ResourceT IO) BL.ByteString)
            -- ^The key for the current download along with the 'Conduit' from
            --  which the contents can be fetched.
download opts url = do
   req <- (opts ^. reqFunc) <$> parseUrl (show url)
   key <- liftIO $ atomically insertSlot
   -- send the request
   res <- http req (opts ^. manager)
   -- update headers
   liftIO $ atomically $ updateSlot key $ insertHeaders res
   -- returns slot + conduit
   let conduit = responseBody res Con.$=+ reportProgressConduit key
   return (key, conduit)
   where
      reportProgressConduit :: Int -> Con.Conduit BS.ByteString (ResourceT IO) BL.ByteString
      reportProgressConduit slot = do
         open <- ConL.peek >$> isJust
         when open $ do
            chunk <- ConB.take 8192
            liftIO $ atomically $ updateSlot slot (\s -> s & downloadBytes +~ fromIntegral (BL.length chunk))
            liftIO $ putStrLn ("chunk downloaded: " L.++ show (BL.length chunk))
            Con.yield chunk
            reportProgressConduit slot

      -- |Inserts a default Download at the smallest value in [0..] that is not
      --  contained in a map. The download will have the current url.
      insertSlot :: STM Int
      insertSlot = do
         let m = opts ^. downloadSlots
         m' <- readTVar m
         let k = if IM.null m' then 0 else (+1) . fst $ IM.findMax m'
         modifyTVar' m $ IM.insert k (def & downloadURL .~ T.pack (show url))
         return k

      updateSlot :: Int -> (Download -> Download) -> STM ()
      updateSlot slot f = modifyTVar' (opts ^. downloadSlots) (IM.adjust f slot)

      insertHeaders :: Response a -> Download -> Download
      insertHeaders r d = d & downloadSize .~ clength
                            & downloadStatus .~ InProgress
         where
            clength :: Maybe Integer
            clength = lookup hContentLength (responseHeaders r)
                      >>= BS.readInteger
                      >$> fst

{-download man reqF url =
   do req' <- parseUrl $ show url
      let req = reqF req'
      res <- withSocketsDo $ httpLbs req man
      putStrLn (show url `append` " downloaded.")
      return $ responseBody res-}

-- source -> conduit (put tvars) ->

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
         content Con.$$+- toStrict Con.=$= ConB.sinkFile fn'

      return $ Just $ fn <.> ext response
   where
      toStrict = Con.awaitForever $ Con.yield . BL.toStrict

      -- downloads a Blob and gets the contents
      action :: FetchResult e i -> ResourceT IO (Con.ResumableSource (ResourceT IO) BL.ByteString)
      action (Blob _ url reqMod) = withTaskLimit (opts ^. taskLimit) $
         download (opts & reqFunc %~ (reqMod.)) url >$> snd

      --gets a ByteString out of a non-Blob leaf
      action (PlainText _ p) = return' $ T.encodeUtf8 p
      action (XmlResult _ p) = return' $ B.encode p
      action (BinaryData _ p) = return' p
      action (Info _ k v) = return' $ encode $ object ["key" .= k, "value" .= v]
      action f@(Failure _ _) = return' $ encode $ action' f (opts ^. maxFailureNodes) 0
      action Inner{} = error "called saveFile with Inner node!"

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

(<<=) Nothing _ = False
(<<=) (Just x) y = x <= y

-- |Creates a new resumable source from a value.
return' :: Monad m => o -> ResourceT IO (Con.ResumableSource m o)
return' = return . Con.newResumableSource . Con.yield

-- creates a JSON object out of a list of mandatory and a list of
-- optional fields
object' xs = object . (xs L.++) . map (\(k,Just v) -> k.=v) . filter (isJust.snd)
