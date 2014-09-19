{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |CLI for the main program.
module CLI where

import Prelude hiding (putStrLn, succ, putStr, getLine, (++))
import qualified Prelude as P

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, append, strip)
import qualified Data.Text as T (map)
import System.FilePath.Posix.Generic

import Galleries.Linear
import Galleries.Tree
import Fetch
import Fetch.Tree
import Galleries.Retrieval
import Helper.String
import System.REPL
import System.REPL.State

-- |The application's state
data AppState = AppState{ -- |Current download directory.
                         pwd::Text,
                         -- |The global connrection manager.
                         manager::Manager,
                         -- |Directory for scripts.
                         scriptDir::Text,
                         -- |The collection of linear scripts.
                         linearScripts::M.Map Text LinearCrawler,
                         -- |The collection of tree scripts.
                         treeScripts::M.Map Text VoidCrawler}

-- |Main function.
mainCLI :: AppState -> IO ()
mainCLI initState =
   do iterateUntilM (`elem` [":e",":exit"]) (iter >=> const prompt) ""
         `runStateT`
          initState
      return ()
   where
      commandLib = [help, comic, tree, gallery, file, cd, prwd, noOp, unknown]
      iter = flip commandDispatch commandLib

-- |Case-insensitive and whitespace-removing 'elem'.
elem' :: Text -> [Text] -> Bool
elem' t ts = clean t `elem` map clean ts
   where clean = strip . T.map toLower

-- |The current program version.
version :: Text
version = "v1.1"

-- |Command for unknown inputs.
unknown :: Command (StateT AppState IO) (Either () ())
unknown = makeCommand "Unknown" (const True) "Unknown command." $
          \cmd -> putErrLn $ "Unknown command '" ++ cmd ++ "'. Type ':help' or ':h'" ++
                             "for a list of available commands or ':e' to exit."

-- |Does nothing.
noOp :: MonadIO m => Command m (Either () ())
noOp = makeCommand "" (`elem'` [""]) "Does nothing." $ const (return ())

-- |Prints the help text.
help :: Command (StateT AppState IO) (Either () ())
help = makeCommand "Help" (`elem'` [":h",":help"]) "Prints this help text." help'
   where
      help' _ = do putStrLn $ "Hephaesthos " ++ version
                   ln
                   putStrLn "CLI interface. Download files en masse."
                   replicateM_ 2 ln
                   cur <- get
                   putStrLn $ "Current download folder: " ++ pwd cur
                   ln
                   putStrLn "Available commands:"
                   putStrLn ":help    -- Prints this message."
                   putStrLn ":e       -- Exits the program."
                   putStrLn ":pwd     -- \"Print working directory\"; shows the current download folder."
                   putStrLn ":cd      -- Changes the current download folder."
                   putStrLn ":comic   -- Downloads a webcomic."
                   putStrLn ":tree    -- Download a tree of files."
                   putStrLn ":gallery -- Downloads a simple gallery (a list of elements"
                   putStrLn "            on a single page."
                   putStrLn ":file    -- Downloads a single file."

-- |Downloads a comic. Command-wrapper for 'downloadComic'.
comic :: Command (StateT AppState IO) (Either () ())
comic = makeCommand1 "Download comic" (`elem'` [":comic"]) "Downloads a comic."
                     comicAsk comic'
   where
      comicAsk = predAsker "Enter comic name (or type ':list' to show all available): "
                           "No comic by that name."
                           comicAsk'

      comicAsk' v = do lc <- liftM linearScripts get
                       let lc' = M.insert ":list" undefined lc
                       return $ M.member v lc'

      comic' _ v = do res <- runOnce (fromVerbatim v) listComics
                      case res of Just _ -> return ()
                                  Nothing -> downloadComic $ fromVerbatim v

-- |Lists all comics.
listComics :: Command (StateT AppState IO) (Either () ())
listComics = makeCommand "List comics" (`elem'` [":list"]) "Lists all available comics."
                        $ const (get1 linearScripts >>= mapM_ putStrLn . M.keys)

-- |Downloads a comic of a given name.
downloadComic :: Text -> StateT AppState IO ()
downloadComic c = do
   c' <- liftM (M.lookup c . linearScripts) get
   (wd, m) <- get2 pwd manager
   case c' of Nothing -> putStrLn "No comic by this name."
              Just v -> void (liftIO $ runExceptT
                              $ getLinearComic m v >>= downloadFiles m wd)

-- |Downloads a single file.
file :: Command (StateT AppState IO) (Either () ())
file = makeCommand1 "Download file" (`elem` [":file"]) "Downloads a single file."
                    fileAsk file'
   where
      fileAsk = predAsker "Enter file URL: "
                          undefined
                          (const $ return True)

      file' _ v = do (wd, m) <- get2 pwd manager
                     liftIO $ runExceptT $ downloadSave m wd $ fromVerbatim v
                     return ()

-- |Changes the download directory.
cd :: Command (StateT AppState IO) (Either () ())
cd = makeCommand1 "Change directory" (`elem'` [":cd"]) "Changes the current directory."
                  cdAsk cd'
   where
      cdAsk = predAsker "Enter new directory (may be relative to current one): "
                        undefined
                        (const $ return True)

      cd' _ v = do (wd,st) <- get2 pwd id
                   let p = normalise $ wd </> fromVerbatim v
                   if isValid p then put $ st{pwd=p}
                   else putErrLn "Couldn't parse path!"

-- |Prints the download directory.
prwd :: Command (StateT AppState IO) (Either () ())
prwd = makeCommand "Print directory" (`elem'` [":pwd"]) "Prints the current directory."
                   $ const (get >>= putStrLn . pwd)

-- |Runs a tree crawler.
tree :: Command (StateT AppState IO) (Either () ())
tree = makeCommand2 "Run tree crawler" (`elem'` [":tree"]) "Runs a tree crawler against a URL."
       treeAsk urlAsk tree'
   where
      treeAsk = predAsker "Enter crawler name (or type ':listTrees' to show all available): "
                          "No crawler by that name."
                          treeAsk'

      treeAsk' v = do tc <- liftM treeScripts get
                      let tc' = M.insert ":listTree" undefined tc
                      return $ M.member v tc'

      urlAsk = predAsker "Enter URL: " undefined (const $ return True)

      tree' _ v url  = do let v' = fromVerbatim v
                          res <- runOnce v' listTrees
                          (wd, m, trees) <- get3 pwd manager treeScripts
                          let cr = fromJust $ M.lookup v' trees
                              succ = crawlerFunction cr undefined
                              tree = fetchTree' m succ $ fromVerbatim url
                              doDownload = liftIO $ runExceptT $ extractBlobs tree
                                                                 >>= downloadFiles m wd
                          case res of Just _ -> return ()
                                      Nothing -> void doDownload

-- |Lists all comics.
listTrees :: Command (StateT AppState IO) (Either () ())
listTrees = makeCommand "List crawlers" (`elem'` [":listTree"])
                        "Lists all available crawlers."
                        $ const (get1 treeScripts >>= mapM_ putStrLn . M.keys)

-- |Downloads a simple gallery.
gallery :: Command (StateT AppState IO) (Either () ())
gallery = makeCommand2 "Download gallery" (`elem'` [":g",":gallery"])
                       "Downloads a list of numbered files."
                       urlAsk numAsk gallery'
   where
      urlAsk = predAsker "Enter URL of the first file: " undefined (const $ return True)

      numAsk = asker "Enter number of items: " err err (return . (>0))
         where err = "Expected positive integer!"

      gallery' _ url num = do let urls = pictureList' (fromVerbatim url) num
                              (wd, m) <- get2 pwd manager
                              liftIO $ runExceptT $ downloadFiles m wd urls
                              return ()

-- |Printd a newline.
ln :: StateT AppState IO ()
ln = putStrLn ""






