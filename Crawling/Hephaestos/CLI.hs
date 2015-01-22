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
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import Data.Char
import qualified Data.Collections as Co
import Data.Dynamic
import Data.Functor.Monadic
import Data.List (inits, partition)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M
import Data.Maybe
import Data.List (foldl1')
import Data.ListLike (ListLike(append))
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
import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Crawlers.Library
import Crawling.Hephaestos.Crawlers.Templates
import Crawling.Hephaestos.Fetch hiding (manager, maxFailureNodes)
import Crawling.Hephaestos.Fetch.Forest
import Crawling.Hephaestos.Fetch.Successor

import Debug.Trace

-- |The application's state
data AppState =
   forall c.(Co.Collection (c (ResultSet [] Dynamic)) (ResultSet [] Dynamic)) =>
   AppState{ -- |Current download directory.
             pwd::FilePath,
             -- |The global connrection manager.
             manager::Manager,
             -- |Directory for scripts.
             appConfig::AppConfig,
             -- |Global request configuration.
             reqConf::RequestConfig,
             -- |The collection of tree scripts.
             crawlers::c (ResultSet [] Dynamic)}

-- |Main function.
mainCLI :: AppState -> IO ()
mainCLI initState =
   -- Run commands until one of them returns True (=quit)
   runIO (iterateUntilM id (const prompt >=> iter) False)
   >> putStrLn ("Quitting..." :: String)
   where
      -- run a command and print errors if necessary
      iter x = commandDispatch x commandLib
               `catchAll` (printError >=> const (return False))

      -- runs a StateT ExceptT IO in IO
      runIO = flip runStateT initState

shortCommandLib :: [Command (StateT AppState IO) Bool]
shortCommandLib = [help, crawler, list, cd, prwd, exit]
commandLib :: [Command (StateT AppState IO) Bool]
commandLib = shortCommandLib P.++ [noOp, unknown]


-- Fluff commands (help, exit, cd, pwd)
-------------------------------------------------------------------------------

-- |Command for unknown inputs.
unknown :: Command (StateT AppState IO) Bool
unknown = makeCommandN "Unknown" (const True) "Unknown command."
                       [] (repeat unknownAsk) unknown'
   where
      unknownAsk :: (MonadIO m, Functor m) => Asker m Verbatim
      unknownAsk = typeAsker "BUG: " ""

      unknown' cmd _ = do
         error $ liftIO
               $ putErrLn $ "Unknown command '" `append` cmd `append` "'. Type ':help' or ':h' " `append`
                            "for a list of available commands or ':e' to exit."
         return False

-- |Does nothing.
noOp :: Command (StateT AppState IO) Bool
noOp = makeCommand "" (`elem'` [""]) "Does nothing." $ const (return False)

-- |Exits the program
exit :: Command (StateT AppState IO) Bool
exit = makeCommand ":[e]xit" (`elem'` [":e", ":exit"]) "Exits the program"
                   $ const (return True)

-- |Prints the help text.
help :: Command (StateT AppState IO) Bool
help = makeCommand ":[h]elp" (`elem'` [":h",":help"]) "Prints this help text." help'
   where
      help' _ = do emphasize $ liftIO $ putStrLn $ "Hephaesthos " `append` version
                   ln
                   liftIO $ putStrLn ("CLI interface. Download files en masse." :: String)
                   ln
                   cur <- get
                   liftIO $ putStrLn $ "Current download folder: " `append` (encodeString $ pwd cur)
                   ln
                   summarizeCommands shortCommandLib
                   return False

-- |Changes the download directory.
cd :: Command (StateT AppState IO) Bool
cd = makeCommand1 ":cd" (`elem'` [":cd"]) "Changes the current directory."
                  cdAsk cd'
   where
      cdAsk = predAsker "Enter new directory (may be relative to current one): "
                        undefined
                        (const $ return True)

      cd' _ v = do (wd,st) <- get2 pwd id
                   path <- (wd </> (fromText' $ fromVerbatim v))
                           |> encodeString
                           |> liftIO . D.canonicalizePath
                           >$> decodeString
                   valid <- liftIO $ validPath path
                   if valid then put $ st{pwd=path}
                   else error $ liftIO $ putErrLn ("Invalid path (incorrect format or no write permissions)!" :: String)
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
         (allM ($ fp) checks) `catchIOError` (const $ return False)
         where
            checks = [return . valid, existingRoot, writable]
            -- |at least some initial part of the path must exist
            existingRoot = mapM doesExist . paths >=$> any id

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
prwd :: Command (StateT AppState IO) Bool
prwd = makeCommand ":pwd" (`elem'` [":pwd"]) "Prints the current directory."
                   prwd'
   where
      prwd' _ = get >>= liftIO . putStrLn . toText' . pwd >> return False


-- The actual meat and bones (run & list crawlers).
-------------------------------------------------------------------------------

-- |Runs a tree crawler.
crawler :: Command (StateT AppState IO) Bool
crawler = makeCommand1 ":[c]rawler" (`elem'` [":c",":crawler"])
                       "Runs a tree crawler against a URL."
                       treeAsk tree'
   where
      treeAsk = predAsker "Enter crawler name (or type ':list' to show all available): "
                          "No crawler by that name."
                          treeAsk'

      treeAsk' :: T.Text -> StateT AppState IO Bool
      treeAsk' v = do
         as <- fetchOptions
         AppState{crawlers=c} <- get
         let match = not $ Co.null $ Co.filter (\x -> commandTest (x as) v) c
         return $ T.strip v /= ":list" && match

      tree' :: T.Text -> Verbatim -> StateT AppState IO Bool
      tree' _ (Verbatim v) =
         do AppState{crawlers=c} <- get
            as <- fetchOptions
            let match = head $ Co.toList
                             $ Co.filter (\x -> commandTest (x as) v) c

            -- if the command wasn't ":list", run a crawler
            res <- runOnce v list
            maybe (do results <- lift $ runCommand (match as) (quoteArg v)
                      report $ liftIO $ putStrLn ("Job done." :: String)
                      return False)
                  (const $ return False)
                  res

-- |Lists all crawlers.
list :: Command (StateT AppState IO) Bool
list = makeCommand ":[l]ist" (`elem'` [":l", ":list"])
                   "Lists all available crawlers."
                   list'
   where
      list' _ = do AppState{crawlers=c} <- get
                   as <- fetchOptions
                   Co.mapM_ (\x -> liftIO $ putStrLn $ commandName (x as)
                                                       `append` " - " `append`
                                                       commandDesc (x as)) c
                   return False

-- Utility
-------------------------------------------------------------------------------
-- |Print a newline.
ln :: MonadIO m => m ()
ln = liftIO $ putStrLn ("" :: String)

-- |Gets the parameters that the crawlers need from the app state.
--  A convenience function to avoid always cluttering code that
--  has to do with crawlers with HList constructions
fetchOptions :: StateT AppState IO FetchOptions
fetchOptions = do
   (m,conf,dir, appConf) <- get4 manager reqConf pwd appConfig
   return $ FetchOptions (createReferer conf)
                         m
                         (runRequestConfig conf)
                         dir
                         (maxFailureNodes appConf)

-- |The current program version.
version :: Text
version = "v1.3.1"
