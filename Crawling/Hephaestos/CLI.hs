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

import Prelude hiding (putStrLn, succ, putStr, getLine, (++), error, FilePath)
import qualified Prelude as P

import Control.Arrow
import Control.Lens ((&), (%~), (^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import Data.Char
import qualified Data.Collections as Co
import Data.Dynamic
import Data.Functor.Monadic
import Data.List (inits, partition, foldl1')
import Data.List.Split (splitOneOf)
import Data.ListLike (ListLike(append))
import qualified Data.Map as M
import Data.Maybe
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Void
import qualified Network.HTTP.Conduit as C
import qualified System.Directory as D
import Filesystem.Path.CurrentOS' hiding (append)
import System.REPL
import System.REPL.Command
import System.REPL.State

import Crawling.Hephaestos.CLI.Color
import Crawling.Hephaestos.CLI.Config
import Crawling.Hephaestos.CLI.Errors
import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Crawlers.Library
import Crawling.Hephaestos.Crawlers.Templates
import Crawling.Hephaestos.Fetch hiding (manager, maxFailureNodes)
import Crawling.Hephaestos.Fetch.Forest
import Crawling.Hephaestos.Fetch.Successor
import qualified Crawling.Hephaestos.Fetch.Types as FT
import Crawling.Hephaestos.Transform (getTransformation)

import Crawling.Hephaestos.I18N

import Debug.Trace

-- |The application's state
data AppState =
   forall c.(Co.Collection (c (ResultSet Ident [] Dynamic)) (ResultSet Ident [] Dynamic)) =>
   AppState{ -- |Current download directory.
             pwd::FilePath,
             -- |The global connrection manager.
             manager::C.Manager,
             -- |Directory for scripts.
             appConfig::AppConfig,
             -- |Global request configuration.
             reqConf::RequestConfig,
             -- |The collection of tree scripts.
             crawlers::c (ResultSet Ident [] Dynamic)}

-- |Main function.
mainCLI :: AppState -> IO ()
mainCLI initState =
   -- Run commands until one of them returns True (=quit)
   runIO (iterateUntilM id (const prompt >=> iter) False)
   >> putStrLn (msg l MsgQuitting)
   where
      l = appConfig initState ^. appLang

      -- run a command and print errors if necessary
      iter x = commandDispatch x (commandLib l)
               `catchIOError` (liftIO . errorMsg l >=> printError >=> const (return False))

      -- runs a StateT ExceptT IO in IO
      runIO = flip runStateT initState

shortCommandLib :: Lang -> [Command (StateT AppState IO) Bool]
shortCommandLib l = map ($ l) [help, crawler, list, trans, cd, prwd, exit]
commandLib :: Lang -> [Command (StateT AppState IO) Bool]
commandLib l = shortCommandLib l P.++ map ($ l) [noOp, unknown]

-- Fluff commands (help, exit, cd, pwd)
-------------------------------------------------------------------------------

-- |Command for unknown inputs.
unknown :: Lang -> Command (StateT AppState IO) Bool
unknown l = makeCommandN (msg l MsgUnknownC) (const True) (msg l MsgUnknownC)
                         [] (repeat unknownAsk) unknown'
   where
      unknownAsk :: (MonadIO m, Functor m) => Asker m Verbatim
      unknownAsk = typeAsker "BUG: " ""

      unknown' cmd _ =
         (liftIO $ error $ putErrLn $ msg l $ MsgUnknownCommand cmd) >> return False

-- |Does nothing.
noOp :: Lang -> Command (StateT AppState IO) Bool
noOp l = makeCommand "" (`elem'` [""]) (msg l MsgNoOpC) $ const (return False)

-- |Exits the program
exit :: Lang -> Command (StateT AppState IO) Bool
exit l = makeCommand ":[e]xit" (`elem'` [":e", ":exit"]) (msg l MsgExitC)
                     $ const (return True)

-- |Prints the help text.
help :: Lang -> Command (StateT AppState IO) Bool
help l = makeCommand ":[h]elp" (`elem'` [":h",":help"]) (msg l MsgHelpC) help'
   where
      help' _ = do
         liftIO $ emphasize $ putStrLn $ msg l $ MsgHelpTitle version
         liftIO $ ln
         liftIO $ putStrLn $ msg l MsgHelpDesc
         liftIO $ ln
         cur <- get
         liftIO $ putStrLn $ msg l $ MsgCurrentDir $ toText' (pwd cur)
         liftIO $ ln
         summarizeCommands $ shortCommandLib l
         return False

-- |Changes the download directory.
cd :: Lang -> Command (StateT AppState IO) Bool
cd l = makeCommand1 ":cd" (`elem'` [":cd"]) (msg l MsgChangeDirC)
                    cdAsk cd'
   where
      cdAsk = predAsker (msg l MsgChangeDirAsk)
                        undefined
                        (const $ return True)

      cd' _ v = do (wd,st) <- get2 pwd id
                   path <- (wd </> fromText' (fromVerbatim v))
                           |> encodeString
                           |> liftIO . D.canonicalizePath
                           >$> decodeString
                   valid <- liftIO $ validPath path
                   if valid then put $ st{pwd=path}
                   else error $ liftIO $ putErrLn $ msg l MsgInvalidPath
                   return False

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
            checks = [return . valid, existingRoot, writable]
            -- |at least some initial part of the path must exist
            existingRoot = mapM doesExist . paths >=$> or

            -- |the last existing part of the path must writable
            --writable = mapM doesExist . paths
            --           >=>  (D.getPermissions . lastExisting)
            --           >=> (\x -> traceM (show x) >> return x)
            --           >=$> D.writable

            -- |Disabled, because 'D.getPermissions' returns false negatives.
            --  It incorrectly asserts some folders (e.g. ~/Downloads),
            --  to be non-writable.
            writable = const $ return True

            -- inits of the filepath
            paths = tail . inits . splitDirectories

            lastExisting = snd . head . dropWhile (not.fst) . reverse

            doesExist = D.doesDirectoryExist . encodeString . foldl1' (</>)

-- |Prints the download directory.
prwd :: Lang -> Command (StateT AppState IO) Bool
prwd l = makeCommand ":pwd" (`elem'` [":pwd"]) (msg l MsgPrintDirC) prwd'
   where
      prwd' _ = get >>= liftIO . putStrLn . toText' . pwd >> return False

-- The actual meat and bones (run & list crawlers).
-------------------------------------------------------------------------------

-- |Runs a tree crawler.
crawler :: Lang -> Command (StateT AppState IO) Bool
crawler l = makeCommand1 ":[c]rawler" (`elem'` [":c",":crawler"])
                         (msg l MsgCrawlerC) treeAsk tree'
   where
      treeAsk = predAsker (msg l $ MsgCrawlerEnterName ":[l]ist")
                          (msg l MsgCrawlerDoesNotExist)
                          treeAsk'

      treeAsk' :: T.Text -> StateT AppState IO Bool
      treeAsk' v = do
         as <- fetchOptions
         AppState{crawlers=c} <- get
         let match = not $ Co.null $ Co.filter (\x -> commandTest (x as) v) c
         return $ T.strip v /= ":list" && T.strip v /= ":l" && match

      tree' :: T.Text -> Verbatim -> StateT AppState IO Bool
      tree' _ (Verbatim v) =
         do AppState{crawlers=c} <- get
            as <- fetchOptions
            let match = head $ Co.toList
                             $ Co.filter (\x -> commandTest (x as) v) c

            -- if the command wasn't ":list", run a crawler
            res <- runOnce v (list l)
            maybe (do results <- lift $ runCommand (match as) (quoteArg v)
                      report $ liftIO $ putStrLn (msg l MsgJobDone)
                      return False)
                  (const $ return False)
                  res

-- |Lists all crawlers.
list :: Lang -> Command (StateT AppState IO) Bool
list l = makeCommand ":[l]ist" (`elem'` [":l", ":list"])
                   (msg l MsgListC) list'
   where
      list' _ = do AppState{crawlers=c} <- get
                   as <- fetchOptions
                   Co.mapM_ (\x -> liftIO $ putStrLn $ commandName (x as)
                                                       `append` " - " `append`
                                                       commandDesc (x as)) c
                   return False

trans :: Lang -> Command (StateT AppState IO) Bool
trans l = makeCommand2 ":[t]rans" (`elem'` [":t", ":trans"])
                     (msg l MsgTransC)
                     mfAsk (transformAsker l Nothing) trans'
   where
      mfAsk = predAsker (msg l MsgTransEnterName)
                        (msg l MsgFileDoesNotExist)
                        (liftIO . D.doesFileExist . T.unpack)

      trans' _ (Verbatim mf) (Just trName) = liftIO $ do
         let tr = getTransformation trName
             (dir, name) = (parent &&& id) . decodeString $ T.unpack mf
         err <- tr dir name
         mapM_ (error . putErrLn . show) err
         report $ putStrLn (msg l MsgJobDone)
         return False

-- Helpers
-------------------------------------------------------------------------------

-- |Print a newline.
ln :: MonadIO m => m ()
ln = liftIO $ putStrLn ("" :: String)

-- |Gets the parameters that the crawlers need from the app state.
--  A convenience function to avoid always cluttering code that
--  has to do with crawlers with HList constructions
fetchOptions :: StateT AppState IO FT.FetchOptions
fetchOptions = do
   (m,conf,dir, appConf) <- get4 manager reqConf pwd appConfig
   return $ FT.FetchOptions (conf ^. createReferer)
                            m
                            (runRequestConfig conf)
                            dir
                            (appConf ^. maxFailureNodes)
                            (appConf ^. threadPoolSize)
                            (appConf ^. saveFetchState)
                            (appConf ^. saveReqMod)

-- |The current program version.
version :: Text
version = "1.5"
