{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |CLI for the main program.
module Crawling.Hephaestos.CLI (mainCLI, AppState(..)) where

import Prelude hiding (putStrLn, succ, putStr, getLine, (++))
import qualified Prelude as P

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import Data.Char
import Data.Either.Unwrap (fromRight)
import Data.List (inits)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, append, strip, pack, unpack)
import qualified Data.Text as T (map)
import qualified Network.HTTP.Conduit as C
import qualified System.Directory as D
import qualified System.FilePath as Px

import Crawling.Hephaestos.CLI.Config
import Crawling.Hephaestos.Crawlers
import Crawling.Hephaestos.Crawlers.Templates
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Tree
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Helper.Functor
import Crawling.Hephaestos.Helper.String
import System.FilePath.Generic
import System.REPL
import System.REPL.Command
import System.REPL.State

import Debug.Trace

-- |The application's state
data AppState = AppState{ -- |Current download directory.
                         pwd::Text,
                         -- |The global connrection manager.
                         manager::Manager,
                         -- |Directory for scripts.
                         appConfig::AppConfig,
                         -- |Global request configuration.
                         reqMod::(C.Request -> C.Request),
                         -- |The collection of linear scripts.
                         linearScripts::M.Map Text (SimpleLinearCrawler (Maybe Int)),
                         -- |The collection of tree scripts.
                         treeScripts::M.Map Text VoidCrawler}

-- |Main function.
mainCLI :: AppState -> IO ()
mainCLI initState =
   do iterateUntilM id (const prompt >=> iter >=> res) False
         `runStateT`
          initState
      return ()
   where
      iter = flip commandDispatch commandLib
      res (Left _) = return False
      res (Right r) = return r

shortCommandLib :: [Command (StateT AppState IO) (Either (AskFailure Text) Bool)]
shortCommandLib = [help, comic, tree, gallery, file, cd, prwd, exit]
commandLib :: [Command (StateT AppState IO) (Either (AskFailure Text) Bool)]
commandLib = shortCommandLib P.++ [noOp, unknown]

-- |Case-insensitive and whitespace-removing 'elem'.
elem' :: Text -> [Text] -> Bool
elem' t ts = clean t `elem` map clean ts
   where clean = strip . T.map toLower

-- |The current program version.
version :: Text
version = "v1.1"

-- |Command for unknown inputs.
unknown :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
unknown = makeCommand "Unknown" (const True) "Unknown command." $
          \cmd -> do putErrLn $ "Unknown command '" ++ cmd ++ "'. Type ':help' or ':h'" ++
                               "for a list of available commands or ':e' to exit."
                     return False

-- |Does nothing.
noOp :: MonadIO m => Command m (Either (AskFailure Text) Bool)
noOp = makeCommand "" (`elem'` [""]) "Does nothing." $ const (return False)

-- |Exits the program
exit :: MonadIO m => Command m (Either (AskFailure Text) Bool)
exit = makeCommand ":[e]xit" (`elem'` [":e", ":exit"]) "Exits the program"
                   $ const (return True)

-- |Prints the help text.
help :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
help = makeCommand ":[h]elp" (`elem'` [":h",":help"]) "Prints this help text." help'
   where
      help' _ = do putStrLn $ "Hephaesthos " ++ version
                   ln
                   putStrLn "CLI interface. Download files en masse."
                   ln
                   cur <- get
                   putStrLn $ "Current download folder: " ++ pwd cur
                   ln
                   summarizeCommands shortCommandLib
                   return False

-- |Downloads a comic. Command-wrapper for 'downloadComic'.
comic :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
comic = makeCommand1 ":comic" (`elem'` [":comic"]) "Downloads a comic."
                     comicAsk comic'
   where
      comicAsk = predAsker "Enter comic name (or type ':list' to show all available): "
                           "No comic by that name."
                           comicAsk'

      comicAsk' v = do lc <- liftM linearScripts get
                       let lc' = M.insert ":list" undefined lc
                       return $ M.member v lc'

      comic' _ (Verbatim v) =
         do (wd, m, req, lc) <- get4 pwd manager reqMod linearScripts
            res <- runOnce v listComics
            let c = lc M.! v
                succ = crawlerFunction c
                tree = fetchTree m succ req Nothing (firstURL c)

            case res of Just _  -> return False
                        Nothing -> runExceptT' (do files <- extractBlobs tree
                                                   downloadFiles m wd files)
                                   >> return False

-- |Lists all comics.
listComics :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
listComics = makeCommand ":list" (`elem'` [":list"]) "Lists all available comics."
                        $ const (get1 linearScripts
                                 >>= mapM_ putStrLn . M.keys
                                 >> return False)

-- |Downloads a single file.
file :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
file = makeCommand1 ":file" (`elem` [":file"]) "Downloads a single file."
                    fileAsk file'
   where
      fileAsk = predAsker "Enter file URL: "
                          undefined
                          (const $ return True)

      file' _ v = do (wd, m, req) <- get3 pwd manager reqMod
                     runExceptT' $ downloadSave m req wd $ fromVerbatim v
                     return False

-- |Changes the download directory.
cd :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
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
                   else putErrLn "Invalid path (incorrect format or no write permissions)!"
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
prwd :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
prwd = makeCommand ":pwd" (`elem'` [":pwd"]) "Prints the current directory."
                   $ const (get >>= putStrLn . pwd >> return False)

-- |Runs a tree crawler.
tree :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
tree = makeCommand2 ":tree" (`elem'` [":tree"]) "Runs a tree crawler against a URL."
       treeAsk urlAsk tree'
   where
      treeAsk = predAsker "Enter crawler name (or type ':listTrees' to show all available): "
                          "No crawler by that name."
                          treeAsk'

      treeAsk' v = do tc <- liftM treeScripts get
                      let tc' = M.insert ":listTree" undefined tc
                      return $ M.member v tc'

      urlAsk = predAsker "Enter URL: " undefined (const $ return True)

      tree' _ (Verbatim v) (Verbatim url) =
         do res <- runOnce v listTrees
            (wd, m, req, trees) <- get4 pwd manager reqMod treeScripts
            let cr = fromJust $ M.lookup v trees
                succ = crawlerFunction cr
                tree = fetchTree' m succ req url
                doDownload = runExceptT' $ extractBlobs tree >>= downloadFiles m wd
            case res of Just _ -> return False
                        Nothing -> doDownload >> return False

-- |Lists all comics.
listTrees :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
listTrees = makeCommand ":listTree" (`elem'` [":listTree"])
                        "Lists all available crawlers."
                        $ const (get1 treeScripts >>= mapM_ putStrLn . M.keys >> return False)

-- |Downloads a simple gallery.
gallery :: Command (StateT AppState IO) (Either (AskFailure Text) Bool)
gallery = makeCommand2 ":[g]allery" (`elem'` [":g",":gallery"])
                       "Downloads a list of numbered files."
                       urlAsk numAsk gallery'
   where
      urlAsk = predAsker "Enter URL of the first file: " undefined (const $ return True)

      numAsk = asker "Enter number of items: " err err (return . (>0))
         where err = "Expected positive integer!"

      gallery' _ (Verbatim url) num =
         do (wd, m, req) <- get3 pwd manager reqMod
            let succ = fileList' num
                tree = fetchTree' m succ req url
            runExceptT' $ extractBlobs tree >>= downloadFiles m wd
            return False

-- |Printd a newline.
ln :: StateT AppState IO ()
ln = putStrLn ""






