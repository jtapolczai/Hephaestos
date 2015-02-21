{-# LANGUAGE OverloadedStrings #-}

-- |CLI for the main program.
module Crawling.Hephaestos.CLI.Status where

import Prelude hiding (putStrLn, putStr, error, FilePath, truncate)
import qualified Prelude as P

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.Utils
import Control.Lens ((&), (%~), (^.))
import Control.Monad (when)
import Data.Functor ((<$>))
import Data.Functor.Monadic
import qualified Data.IntMap as IM
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Filesystem.Path.CurrentOS' hiding (append)
import System.Console.ANSI
import System.REPL
import Text.Printf (printf)

import Crawling.Hephaestos.CLI.Color
import Crawling.Hephaestos.CLI.Config
import Crawling.Hephaestos.CLI.Errors
import Crawling.Hephaestos.Fetch
import qualified Crawling.Hephaestos.Fetch.Types as FT

import Crawling.Hephaestos.I18N

import Debug.Trace

-- |Runs a status monitor which calls 'clearDownloads', followed by 'printDownloads',
--  every n milliseconds. This function will run until a TVar is changed.
--
--  This function uses 'threadDelay' and should therefore be run in its own thread.
runStatusMonitor :: Lang
                 -> FT.FetchOptions
                 -> STM Bool
                    -- ^The TVar that indicates termination. When this is
                    --  'True', 'runStatusMonitor' will stop running
                    --  before printing its next output.
                    --
                    --  Since 'printDownloads' prints to stdout, it is better
                    --  to set this TVar instead of terminating its thread, as
                    --  that might leave half-finished output.
                 -> Int
                    -- ^The n milliseconds to wait between every call of
                    --  'printDownloads'.
                 -> Int
                    -- ^The maximum number of currently running downloads to
                    --  display.
                 -> Int
                    -- ^Width of the terminal.
                 -> IO ()
runStatusMonitor l opts terminate wait maxLines maxColumns = go
   where
      go = do threadDelay (wait*1000)
              doTerminate <- atomically terminate
              when (not doTerminate) $ do
                 atomically $ clearDownloads opts
                 printDownloads l opts maxLines maxColumns
                 go

-- |Clears the finished and failed downloads. By "cleared", I mean that all
--  elements in the maps which hold the failed/finished tasks have all their
--  elements removed. In addition, the number of thus removed elements is
--  added to the 'FT.downloadStats' field of 'FT.FetchOptions'.
clearDownloads :: FT.FetchOptions
               -> STM ()
clearDownloads opts = do
   cats <- getTasks (opts ^. FT.downloadCategories)
   clearTasks (opts ^. FT.downloadCategories) finishedTasks
   clearTasks (opts ^. FT.downloadCategories) failedTasks

   let numFinished = maybe 0 IM.size $ finishedTasks `M.lookup` cats
       numFailed   = maybe 0 IM.size $ failedTasks `M.lookup` cats
   modifyTVar' (opts ^. FT.downloadStats)
               (M.adjust (numFailed + ) failedTasks
                . M.adjust (numFinished + ) finishedTasks)

-- |Prints the number of successful/failed downloads on stdout (without clearing
--  it).
printDownloadsSummary :: Lang
                      -> FT.FetchOptions
                      -> Bool
                      -- ^Whether to print the "running downloads" line.
                      -> IO ()
printDownloadsSummary l opts printCurrent = do
   stats <- atomically $ readTVar $ opts ^. FT.downloadStats
   let na = msg l MsgTaskStatNotAvailable

       numRunning = maybe na (T.pack . show) $ M.lookup downloadingTasks stats
       numFinished = maybe na (T.pack . show) $ M.lookup finishedTasks stats
       numFailed = maybe na (T.pack . show) $ M.lookup failedTasks stats
   putStrLn $ msg l $ MsgTasksFinished numFinished
   putStrLn $ msg l $ MsgTasksFailed numFailed
   when printCurrent $ putStrLn $ msg l $ MsgTasksRunning numRunning


-- |Clears the console and prints the statuses of the current downloads.
--  Besides the console output, this function doesn't change anything (including
--  any TVars).
printDownloads :: Lang
               -> FT.FetchOptions
               -> Int
                  -- ^The maximum amount of currently running downloads to
                  --  display. Keep in mind that 3-4 additional lines will be
                  --  used for displaying the number of finished/failed/running
                  --  downloads, and for an "n omitted" message if not all
                  --  downloads can be displayed.
               -> Int
                  -- ^Width of the terminal. All lines will be truncated to this.
                  --  First, the url is truncated. If that is not enough, the
                  --  line as a whole will be truncated.
               -> IO ()
printDownloads l opts maxLines maxColumns = do
   current <- atomically $ getTasks $ opts ^. FT.downloadCategories
   let tasks = M.lookup downloadingTasks current
               >$> IM.assocs
               |> maybe ([],0) (take maxLines &&& (subtract maxLines . length))
   clearScreen
   setCursorPosition 0 0
   printDownloadsSummary l opts True

   mapM_ printTask (fst tasks)

   when (snd tasks > 0) $ putStrLn $ msg l $ MsgTasksOmitted $ snd tasks

   where
      printTask (k, v) = putStrLn $ truncate maxColumns (preURL ++ url ++ postURL)
         where
            preURL = show k ++ " "
            postURL = " " ++ toB (v ^. FT.downloadBytes)
                          ++ maybe "" toB (v ^. FT.downloadSize)

            url = truncate (max 0 (maxColumns - length preURL - length postURL))
                           (show $ v ^. FT.downloadURL)


-- Helpers
-------------------------------------------------------------------------------

truncate m l | length l <= m = l
             | otherwise     = take (m - 3) l ++ "..."


toB :: Integral a => a -> String
toB x = printf "%.3f" x' ++ (prefixes !! i)
   where (i, x') = getPow 0 $ fromIntegral x

getPow :: Int -> Double -> (Int, Double)
getPow i x | i+1 >= length prefixes = (i,x)
           | x < 1024               = (i,x)
           | otherwise              = getPow (i+1) (x / 1024)

prefixes = ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]
