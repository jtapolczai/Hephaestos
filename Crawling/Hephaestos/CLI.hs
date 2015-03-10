{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

-- |CLI for the main program.
module Crawling.Hephaestos.CLI (
   mainCLI,
   AppState(..),
   ) where

import Prelude hiding (putStrLn, succ, putStr, (++), error, FilePath)

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.Concurrent.STM
import Control.Concurrent.STM.Utils
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import qualified Data.Collections as Co
import Data.Dynamic
import Data.Either.Combinators (fromRight')
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.ListLike (ListLike(append))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Network.HTTP.Conduit as C
import qualified System.Directory as D
import Filesystem.Path.CurrentOS' hiding (append)
import System.Console.ANSI
import qualified System.Log.Logger as Log
import qualified System.Log.Handler.Log4jXML as Log
import System.REPL
import System.REPL.Command
import System.REPL.State

import Crawling.Hephaestos.Fetch (downloadingTasks, finishedTasks, failedTasks)
import Crawling.Hephaestos.CLI.Format
import Crawling.Hephaestos.CLI.Config
import Crawling.Hephaestos.CLI.Errors
import Crawling.Hephaestos.CLI.Status
import Crawling.Hephaestos.Crawlers.Library
import qualified Crawling.Hephaestos.Fetch.Types as FT
import Crawling.Hephaestos.I18N
import Crawling.Hephaestos.Transform (getTransformation)

-- |The application's state
data AppState =
   AppState{ -- |Current download directory.
             pwd::FilePath,
             -- |The global connrection manager.
             manager::C.Manager,
             -- |Directory for scripts.
             appConfig::AppConfig,
             -- |Global request configuration.
             reqConf::FT.RequestConfig,
             -- |The collection of tree scripts.
             crawlers::[ResultSet Ident [] Dynamic],
             -- |The collection of running tasks.
             tasks :: TaskCategories FT.TaskCat FT.Download,
             -- |Statistics about running/finished/failed tasks.
             taskStats :: TVar (M.Map FT.TaskCat Int),
             -- |The global task limit.
             taskLimit::TaskLimit,
             -- |The escaping scheme that should be used for filenames
             escapingFunction::Escaping}

-- |Main function.
mainCLI :: AppState -> IO ()
mainCLI initState = do
   -- initialize log4j
   logH <- Log.log4jFileHandler "log.xml" Log.DEBUG
   Log.updateGlobalLogger Log.rootLoggerName (Log.addHandler logH)
   -- Run commands until one of them returns True (=quit)
   flip runStateT initState (do
      lib <- commandLib
      makeREPL (noOp l : lib) (exit l) (unknown l) prompt [Handler handler])
   putStrLn (msg l MsgQuitting)
   where
      l = appConfig initState ^. appLang

      handler :: MonadIO m => SomeException -> m ()
      handler = liftIO . errorMsg l >=> printError


commandLib :: StateT AppState IO [Command (StateT AppState IO) T.Text ()]
commandLib = do
   (l, cs) <- get2 (_appLang . appConfig) crawlers
   return $ map ($ l) [help, list, crawlerComm cs, trans, cd, prwd]

-- Fluff commands (help, exit, cd, pwd)
-------------------------------------------------------------------------------

-- |Command for unknown inputs.
unknown :: Lang -> Command (StateT AppState IO) T.Text ()
unknown l = makeCommandN (msg l MsgUnknownC) (const True) (msg l MsgUnknownC) True
                         [] (repeat unknownAsk) unknown'
   where
      unknownAsk :: (MonadIO m, Functor m) => Asker m Verbatim
      unknownAsk = typeAsker "BUG: " ""

      unknown' cmd _ = liftIO (error $ putErrLn $ msg l $ MsgUnknownCommand cmd)

-- |Does nothing.
noOp :: Lang -> Command (StateT AppState IO) T.Text ()
noOp l = makeCommand "" (`elem'` [""]) (msg l MsgNoOpC) (const $ return ())

-- |Exits the program
exit :: Lang -> Command (StateT AppState IO) T.Text ()
exit l = makeCommand ":[e]xit" (`elem'` [":e", ":exit"]) (msg l MsgExitC)
                     $ const (liftIO Log.removeAllHandlers)

-- |Prints the help text.
help :: Lang -> Command (StateT AppState IO) T.Text ()
help l = makeCommand ":[h]elp" (`elem'` [":h",":help"]) (msg l MsgHelpC) help'
   where
      help' _ = do
         lib <- commandLib
         liftIO $ emphasize $ putStrLn $ msg l $ MsgHelpTitle version
         liftIO ln
         liftIO $ putStrLn $ msg l MsgHelpDesc
         liftIO ln
         cur <- get
         liftIO $ putStrLn $ msg l $ MsgCurrentDir $ toText' (pwd cur)
         liftIO ln
         summarizeCommands lib

-- |Changes the download directory.
cd :: Lang -> Command (StateT AppState IO) T.Text ()
cd l = makeCommand1 ":cd" (`elem'` [":cd"]) (msg l MsgChangeDirC) True cdAsk cd'
   where
      cdAsk = predAsker (msgs l MsgChangeDirAsk)
                        undefined
                        (const $ return True)

      cd' _ v = do (wd,st) <- get2 pwd id
                   path <- (wd </> fromText' (fromVerbatim v))
                           |> encodeString
                           |> liftIO . D.canonicalizePath
                           >$> decodeString
                   isValid <- liftIO $ validPath path
                   if isValid then put $ st{pwd=path}
                   else error $ liftIO $ putErrLn $ msg l MsgInvalidPath

      -- |Returns whether a given @path@ is valid in the following sense:
      --  * @isValid path@ returns true,
      --  * at least some initial part of the path exists,
      --  * the application has write permission to the last existing part
      --    of the path.
      --
      --  IO errors are caught and result in @False@.
      validPath :: FilePath -> IO Bool
      validPath fp =
         allM ($ fp) checks `catchIOError` const (return False)
         where
            checks = [return . valid, existingRoot]
            -- |at least some initial part of the path must exist
            existingRoot = mapM doesExist . paths >=$> or
            -- inits of the filepath
            paths = tail . L.inits . splitDirectories
            doesExist = D.doesDirectoryExist . encodeString . L.foldl' (</>) empty

-- |Prints the download directory.
prwd :: Lang -> Command (StateT AppState IO) T.Text ()
prwd l = makeCommand ":pwd" (`elem'` [":pwd"]) (msg l MsgPrintDirC) prwd'
   where
      prwd' _ = get >>= liftIO . putStrLn . toText' . pwd

-- The actual meat and bones (run & list crawlers).
-------------------------------------------------------------------------------

-- |The "root" crawler command. It takes one parameter (the crawler) name,
--  but doesn't consume it, meaning that it will be available to other command
--  that are bound after it.
--  Its return values can be used as input to crawlers from the
--  "Crawling.Hephaestos.Library" module.
crawler :: Lang -> Command (StateT AppState IO) T.Text (TVar TaskStatus, FT.FetchOptions)
crawler l = Command ":[c]rawler" (`elem` [":c", ":crawler"]) (msg l MsgCrawlerC)
                    crawler'
   where
      crawlerAsk = predAsker (msgs l $ MsgCrawlerEnterName ":[l]ist")
                             (msg l MsgCrawlerDoesNotExist)
                             crawlerAsk'
      crawlerAsk' v = do
         as <- fetchOptions
         AppState{crawlers=c} <- get
         let match = not $ Co.null $ Co.filter (\x -> commandTest (x as undefined) v) c
             v' = T.strip v
         return $ v' == ":list" || v' == ":l" || match

      crawler' args = do let x0 = fromMaybe mempty (L.head args)
                         (Verbatim x1) <- ask crawlerAsk (args L.!! 1)
                         res <- atomicallyM $ newTVar TaskBeginning
                         opts <- fetchOptions
                         -- If no crawler name was given (length args < 2),
                         -- we append the one for which we asked (x1).
                         let args' = if L.length args < 2 then args L.++ [x1] else args
                         return ((res, opts), L.drop 1 args')

-- Creates a huge subcommand for the crawlers.
-- The core problem is that the crawlers take two arguments before they
-- return a Command: the FetchOptions and a function of type @STM ()@.
-- The solution here is to have 'crawler' (the root command) return
-- these two values and pass them some concrete crawler (the subcommand)
-- via monadic bind (>>-).
crawlerComm cs l = subcommand (crawler l) (listC : map wrapper cs)
   where
      -- the ":list" command, in case the use wants to list the crawlers.
      listC _ = list l

      wrapper c (v,opts) = let c' = c opts (makeForkSignal v) in
         c'{runPartialCommand = wrapper' opts v (runPartialCommand c')}

      wrapper' :: FT.FetchOptions
               -> TVar TaskStatus
               -> ([Text] -> IO (a,[Text])) -- ^Old command function.
               -> ([Text] -> StateT AppState IO ((), [Text])) -- ^New, wrapped command function.
      wrapper' opts v cmd args = do
         -- get all sorts of options
         config <- get >$> appConfig
         ts <- get >$> taskStats
         let w = config ^. termWidth
             h = config ^. minTermHeight
             cond = isEmptyTMVar >=$> not
             freq = config ^. screenUpdateFrequency
             simple = config ^. useSingleScreen
             monitor = if simple then runSimpleStatusMonitor
                                 else runStatusMonitor

         --start the command
         --Notice that cmd already got its signal function above, in 'wrapper'
         (_,res) <- forkDelayedSignal v (cmd args)
         --re-throw an exception, if there was one. If not, proceed.
         ex <- atomicallyM $ tryReadTMVar res
         case ex of Just (Left ex') -> throwM ex'; _ -> return ()

         -- start the status monitor
         finishedMon <- atomicallyM $ newTVar False
         (_,monRes) <- fork (do monitor l opts (cond res) freq h w
                                atomically (writeTVar finishedMon True))

         -- block until the command (and status monitor) finish
         _ <- atomicallyM $ readTMVar res
         atomicallyM $ readTVar finishedMon >>= check

         -- clear up
         atomicallyM $ writeTVar ts
                     $ M.fromList [(downloadingTasks, 0),
                                   (failedTasks, 0),
                                   (finishedTasks, 0)]
         liftIO $ clearScreen
         liftIO $ setCursorPosition 0 0

         -- report anything bad that might have happened in the child threads.
         -- at this point, we wait for the termination of the download.
         (ex,ex') <- atomicallyM $ (,) <$> readTMVar res <*> tryReadTMVar monRes
         case ex  of Left e        -> throwM e; _ -> return ()
         case ex' of Just (Left e) -> throwM e; _ -> return ()

         -- if everything went well, we report "job done" and return the
         -- arguments our child thread leaf unconsumed.
         report $ liftIO $ putStrLn (msg l MsgJobDone)
         return ((), snd $ fromRight' ex)

-- |Lists all crawlers.
list :: Lang -> Command (StateT AppState IO) T.Text ()
list l = makeCommand ":[l]ist" (`elem'` [":l", ":list"])
                   (msg l MsgListC) list'
   where
      list' _ = do AppState{crawlers=c} <- get
                   as <- fetchOptions
                   Co.mapM_ (\x -> liftIO $ putStrLn $ commandName (x as undefined)
                                                       `append` " - " `append`
                                                       commandDesc (x as undefined)) c

trans :: Lang -> Command (StateT AppState IO) T.Text ()
trans l = makeCommand2 ":[t]rans" (`elem'` [":t", ":trans"])
                     (msg l MsgTransC) True mfAsk (transformAsker l Nothing)
                     trans'
   where
      mfAsk = predAsker (msgs l MsgTransEnterName)
                        (msg l MsgFileDoesNotExist)
                        (liftIO . D.doesFileExist . T.unpack)

      trans' _ (Verbatim mf) (Just trName) = do
         esc <- get >$> escapingFunction
         let tr = getTransformation trName esc
             (dir, name) = (parent &&& id) . decodeString $ T.unpack mf
         err <- liftIO $ tr dir name
         liftIO $ mapM_ (error . putErrLn . show) err
         liftIO $ report $ putStrLn (msg l MsgJobDone)

-- Helpers
-------------------------------------------------------------------------------

-- |Print a newline.
ln :: MonadIO m => m ()
ln = liftIO $ putStrLn ("" :: String)

-- |Gets the parameters that the crawlers need from the app state.
--  A convenience function to avoid always cluttering code that
--  has to do with crawlers.
fetchOptions :: StateT AppState IO FT.FetchOptions
fetchOptions = do
   (m, conf, dir, appConf) <- get4 manager reqConf pwd appConfig
   (t, ts, tl, esc) <- get4 tasks taskStats taskLimit escapingFunction
   return $ FT.FetchOptions (appConf ^. createReferer)
                            m
                            conf
                            dir
                            (appConf ^. maxFailureNodes)
                            (appConf ^. threadPoolSize)
                            (appConf ^. saveFetchState)
                            (appConf ^. saveReqMod)
                            t
                            ts
                            tl
                            esc

-- |The current program version.
version :: Text
version = "1.5"
