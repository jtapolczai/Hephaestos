{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

-- |CLI for the main program.
module Crawling.Hephaestos.CLI (
   mainCLI,
   AppState(..),
   FetchTreeArgs,) where

import Prelude hiding (putStrLn, succ, putStr, getLine, (++))
import qualified Prelude as P

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import Data.Char
import Data.Dynamic
import Data.Either.Unwrap (fromRight)
import Data.Functor.Monadic
import Data.HList.HList
import Data.List (inits, partition)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, append, strip, pack, unpack)
import qualified Data.Text as T
import Data.Types.Isomorphic
import Data.Void
import qualified Network.HTTP.Conduit as C
import qualified System.Directory as D
import qualified System.FilePath as Px
import System.FilePath.Generic
import System.REPL
import System.REPL.Command
import System.REPL.State

import Crawling.Hephaestos.CLI.Config
import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Crawlers.Library (PackedCrawler)
import Crawling.Hephaestos.Crawlers.Templates
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Helper.String

import Debug.Trace

-- |The application's state
data AppState =
   AppState{ -- |Current download directory.
             pwd::Text,
             -- |The global connrection manager.
             manager::Manager,
             -- |Directory for scripts.
             appConfig::AppConfig,
             -- |Global request configuration.
             reqMod::(C.Request -> C.Request),
             -- |The collection of tree scripts.
             crawlers::M.Map Text (Text, PackedCrawler ErrorIO')}

-- |Main function.
mainCLI :: AppState -> IO ()
mainCLI initState =
   -- Run commands until one of them returns True (=quit)
   runIO (iterateUntilM id (const prompt >=> iter) False)
   -- Finish by reporting errors or displaying an exit message
   >>= either printError (const $ putStrLn ("Quitting..." :: String))
   where
      -- run a command and print errors if necessary
      iter x = commandDispatch x commandLib
               `catchError` (printError >=> const (return False))

      -- runs a StateT ExceptT IO in IO
      runIO = runExceptT . flip runStateT initState

shortCommandLib :: [Command (StateT AppState ErrorIO') Bool]
shortCommandLib = [help, crawler, list, cd, prwd, exit]
commandLib :: [Command (StateT AppState ErrorIO') Bool]
commandLib = shortCommandLib P.++ [noOp, unknown]

-- |Case-insensitive and whitespace-removing 'elem'.
elem' :: Text -> [Text] -> Bool
elem' t ts = clean t `elem` map clean ts
   where clean = strip . T.map toLower

-- |The current program version.
version :: Text
version = "v1.1"

-- |Command for unknown inputs.
unknown :: Command (StateT AppState ErrorIO') Bool
unknown = makeCommandN "Unknown" (const True) "Unknown command."
                       [] (repeat unknownAsk) unknown'

   where
      unknownAsk :: (MonadIO m, Functor m) => Asker m Verbatim
      unknownAsk = typeAsker "BUG: " ""

      unknown' cmd _ = do
         putErrLn $ "Unknown command '" ++ cmd ++ "'. Type ':help' or ':h' " ++
                    "for a list of available commands or ':e' to exit."
         return False

-- |Does nothing.
noOp :: Command (StateT AppState ErrorIO') Bool
noOp = makeCommand "" (`elem'` [""]) "Does nothing." $ const (return False)

-- |Exits the program
exit :: Command (StateT AppState ErrorIO') Bool
exit = makeCommand ":[e]xit" (`elem'` [":e", ":exit"]) "Exits the program"
                   $ const (return True)

-- |Prints the help text.
help :: Command (StateT AppState ErrorIO') Bool
help = makeCommand ":[h]elp" (`elem'` [":h",":help"]) "Prints this help text." help'
   where
      help' _ = do putStrLn $ "Hephaesthos " ++ version
                   ln
                   putStrLn ("CLI interface. Download files en masse." :: String)
                   ln
                   cur <- get
                   putStrLn $ "Current download folder: " ++ pwd cur
                   ln
                   summarizeCommands shortCommandLib
                   return False

-- |Changes the download directory.
cd :: Command (StateT AppState ErrorIO') Bool
cd = makeCommand1 ":cd" (`elem'` [":cd"]) "Changes the current directory."
                  cdAsk cd'
   where
      cdAsk = predAsker "Enter new directory (may be relative to current one): "
                        undefined
                        (const $ return True)

      canonicalizePath = pack <$=< D.canonicalizePath . unpack

      cd' _ v = do (wd,st) <- get2 pwd id
                   p <- liftIO $ canonicalizePath (wd </> fromVerbatim v)
                                 >$> normalise
                   (Right valid) <- liftIO $ runExceptT $ validPath $ unpack p
                   if valid then put $ st{pwd=p}
                   else putErrLn ("Invalid path (incorrect format or no write permissions)!" :: String)
                   return False

      -- |Returns whether a given @path@ is valid in the following sense:
      --  * @isValid path@ returns true,
      --  * at least some initial part of the path exists,
      --  * the application has write permission to the last existing part
      --    of the path.
      --
      --  IO errors are caught and result in @False@.
      validPath :: FilePath -> ErrorIO Bool
      validPath fp =
         catchIO (pack fp) FileError (allM ($ fp) checks) `catchError`
                                                          (const $ return undefined)

         where
            checks = [return . Px.isValid, existingRoot, writable]
            -- |at least some initial part of the path must exist
            existingRoot = mapM doesExist . paths >=$> any fst

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
            paths = tail . inits . splitOneOf Px.pathSeparators

            lastExisting = snd . head . dropWhile (not.fst) . reverse

            doesExist x = do let x' = Px.joinPath x
                             ex <- D.doesDirectoryExist x'
                             return (ex,x')

-- |Prints the download directory.
prwd :: Command (StateT AppState ErrorIO') Bool
prwd = makeCommand ":pwd" (`elem'` [":pwd"]) "Prints the current directory."
                   $ const (get >>= putStrLn . pwd >> return False)

-- |Runs a tree crawler.
crawler :: Command (StateT AppState ErrorIO') Bool
crawler = makeCommandN ":[c]rawler" (`elem'` [":c",":crawler"])
                       "Runs a tree crawler against a URL."
                       [treeAsk] [urlAsk] tree'
   where
      treeAsk = predAsker "Enter crawler name (or type ':list' to show all available): "
                          "No crawler by that name."
                          $ \v -> get >$> crawlers
                                      >$> M.insert ":list" undefined
                                      >$> M.member v

      urlAsk = predAsker "Enter URL: " undefined (const $ return True)

      (|-|) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
      (|-|) f g x = f x || g x

      tree' _ ((Verbatim v):xs) =
         do res <- runOnce v list
            (wd, m, req, trees) <- get4 pwd manager reqMod crawlers

            case res of
               Just _ -> return False
               Nothing ->
                  -- IF the command wasn't ":list", then ask for the URL
                  -- (unless it was already provided as an optional argument)
                  do (Verbatim url) <- if null xs then ask' urlAsk
                                       else return $ head xs

                     --  set up arguments and do the download
                     let crawler = snd $ fromJust $ M.lookup v trees
                         tree = crawler (HCons m (HCons req (HCons url HNil)))

                         results =
                           tree
                           >>= extractResults
                           >$> filter ((isBlob |-| isFailure) . nodeRes)
                           >$> partition (isBlob.nodeRes)
                           >$> (map $ fromBlob . nodeRes &&& nodeReqMod)
                           *** (map $ failureError . nodeRes)


                     lift $ runExceptT' $ do (urls, errs) <- results
                                             printErrors errs
                                             downloadFiles m wd urls
                     return False

-- |Lists all crawlers.
list :: Command (StateT AppState ErrorIO') Bool
list = makeCommand ":[l]ist" (`elem'` [":l", ":list"])
                   "Lists all available crawlers."
                   $ const (get1 crawlers
                            >$> M.assocs
                            >>= mapM_ showCrawler
                            >>  return False)
   where
      showCrawler (name, (desc,_)) =
         if T.null desc then putStrLn   name
                        else putStrLn $ name ++ " - " ++ desc


-- |Print a newline.
ln :: MonadIO m => m ()
ln = putStrLn ("" :: String)

