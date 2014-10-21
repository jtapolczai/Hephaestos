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
   AppState'(..),
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
import Data.List (inits)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, append, strip, pack, unpack)
import qualified Data.Text as T (map)
import Data.Types.Isomorphic
import Data.Void
import qualified Network.HTTP.Conduit as C
import qualified System.Directory as D
import qualified System.FilePath as Px

import Crawling.Hephaestos.CLI.Config
import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Crawlers.Templates
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Helper.String
import System.FilePath.Generic
import System.REPL
import System.REPL.Command
import System.REPL.State

import Debug.Trace

-- |The application's state
data AppState' m =
   AppState{ -- |Current download directory.
             pwd::Text,
             -- |The global connrection manager.
             manager::Manager,
             -- |Directory for scripts.
             appConfig::AppConfig,
             -- |Global request configuration.
             reqMod::(C.Request -> C.Request),
             -- |The collection of tree scripts.
             treeScripts::M.Map Text (HList FetchTreeArgs -> m (MTree ErrorIO' (SuccessorNode SomeException Dynamic)))}

type AppState = AppState' ErrorIO'

type FetchTreeArgs = [Manager, (C.Request -> C.Request), URL]

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
shortCommandLib = [help, tree, file, cd, prwd, exit]
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
         putErrLn $ "Unknown command '" ++ cmd ++ "'. Type ':help' or ':h'" ++
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

-- |Downloads a single file.
file :: Command (StateT AppState ErrorIO') Bool
file = makeCommand1 ":file" (`elem` [":file"]) "Downloads a single file."
                    fileAsk file'
   where
      fileAsk = predAsker "Enter file URL: "
                          undefined
                          (const $ return True)

      file' _ v = do (wd, m, req) <- get3 pwd manager reqMod
                     lift $ runExceptT' $ downloadSave m req wd $ fromVerbatim v
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
tree :: Command (StateT AppState ErrorIO') Bool
tree = makeCommand2 ":tree" (`elem'` [":tree"]) "Runs a tree crawler against a URL."
       treeAsk urlAsk tree'
   where
      treeAsk = predAsker "Enter crawler name (or type ':listTrees' to show all available): "
                          "No crawler by that name."
                          treeAsk'

      treeAsk' v = do tc <- get >$> treeScripts
                      let tc' = M.insert ":listTree" undefined tc
                      return $ M.member v tc'

      urlAsk = predAsker "Enter URL: " undefined (const $ return True)

      tree' _ (Verbatim v) (Verbatim url) =
         do res <- runOnce v listTrees
            (wd, m, req, trees) <- get4 pwd manager reqMod treeScripts
            let crawler = fromJust $ M.lookup v trees
                tree = crawler (HCons m (HCons req (HCons url HNil)))
                doDownload = runExceptT' $ tree
                                           >>= extractBlobs
                                           >>= downloadFiles m wd
            case res of Just _ -> return False
                        Nothing -> lift doDownload >> return False

-- |Lists all comics.
listTrees :: Command (StateT AppState ErrorIO') Bool
listTrees = makeCommand ":listTree" (`elem'` [":listTree"])
                        "Lists all available crawlers."
                        $ const (get1 treeScripts
                                 >>= M.keys
                                 |> mapM_ putStrLn
                                 >> return False)

-- |Print a newline.
ln :: MonadIO m => m ()
ln = putStrLn ("" :: String)

