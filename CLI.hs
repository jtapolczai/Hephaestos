{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |CLI for the main program.
module CLI (mainCLI, AppState(..)) where

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
   do iterateUntilM id (const prompt >=> iter >=> res) False
         `runStateT`
          initState
      return ()
   where
      iter = flip commandDispatch commandLib
      res (Left _) = return False
      res (Right r) = return r

shortCommandLib :: [Command (StateT AppState IO) (Either () Bool)]
shortCommandLib = [help, comic, tree, gallery, file, cd, prwd, exit]
commandLib :: [Command (StateT AppState IO) (Either () Bool)]
commandLib = shortCommandLib P.++ [noOp, unknown]

-- |Case-insensitive and whitespace-removing 'elem'.
elem' :: Text -> [Text] -> Bool
elem' t ts = clean t `elem` map clean ts
   where clean = strip . T.map toLower

-- |The current program version.
version :: Text
version = "v1.1"

-- |Command for unknown inputs.
unknown :: Command (StateT AppState IO) (Either () Bool)
unknown = makeCommand "Unknown" (const True) "Unknown command." $
          \cmd -> do putErrLn $ "Unknown command '" ++ cmd ++ "'. Type ':help' or ':h'" ++
                               "for a list of available commands or ':e' to exit."
                     return False

-- |Does nothing.
noOp :: MonadIO m => Command m (Either () Bool)
noOp = makeCommand "" (`elem'` [""]) "Does nothing." $ const (return False)

-- |Exits the program
exit :: MonadIO m => Command m (Either () Bool)
exit = makeCommand ":[e]xit" (`elem'` [":e", ":exit"]) "Exits the program"
                   $ const (return True)

-- |Prints the help text.
help :: Command (StateT AppState IO) (Either () Bool)
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
comic :: Command (StateT AppState IO) (Either () Bool)
comic = makeCommand1 ":comic" (`elem'` [":comic"]) "Downloads a comic."
                     comicAsk comic'
   where
      comicAsk = predAsker "Enter comic name (or type ':list' to show all available): "
                           "No comic by that name."
                           comicAsk'

      comicAsk' v = do lc <- liftM linearScripts get
                       let lc' = M.insert ":list" undefined lc
                       return $ M.member v lc'

      comic' _ v = do res <- runOnce (fromVerbatim v) listComics
                      case res of Just _ -> return False
                                  Nothing -> do downloadComic $ fromVerbatim v
                                                return False

-- |Lists all comics.
listComics :: Command (StateT AppState IO) (Either () Bool)
listComics = makeCommand ":list" (`elem'` [":list"]) "Lists all available comics."
                        $ const (get1 linearScripts
                                 >>= mapM_ putStrLn . M.keys
                                 >> return False)

-- |Downloads a comic of a given name.
downloadComic :: Text -> StateT AppState IO ()
downloadComic c = do
   c' <- liftM (M.lookup c . linearScripts) get
   (wd, m) <- get2 pwd manager
   case c' of Nothing -> putStrLn "No comic by this name."
              Just v -> void (liftIO $ runExceptT
                              $ getLinearComic m v >>= downloadFiles m wd)

-- |Downloads a single file.
file :: Command (StateT AppState IO) (Either () Bool)
file = makeCommand1 ":file" (`elem` [":file"]) "Downloads a single file."
                    fileAsk file'
   where
      fileAsk = predAsker "Enter file URL: "
                          undefined
                          (const $ return True)

      file' _ v = do (wd, m) <- get2 pwd manager
                     liftIO $ runExceptT $ downloadSave m wd $ fromVerbatim v
                     return False

-- |Changes the download directory.
cd :: Command (StateT AppState IO) (Either () Bool)
cd = makeCommand1 ":cd" (`elem'` [":cd"]) "Changes the current directory."
                  cdAsk cd'
   where
      cdAsk = predAsker "Enter new directory (may be relative to current one): "
                        undefined
                        (const $ return True)

      cd' _ v = do (wd,st) <- get2 pwd id
                   let p = normalise $ wd </> fromVerbatim v
                   if isValid p then put $ st{pwd=p}
                   else putErrLn "Couldn't parse path!"
                   return False

-- |Prints the download directory.
prwd :: Command (StateT AppState IO) (Either () Bool)
prwd = makeCommand ":pwd" (`elem'` [":pwd"]) "Prints the current directory."
                   $ const (get >>= putStrLn . pwd >> return False)

-- |Runs a tree crawler.
tree :: Command (StateT AppState IO) (Either () Bool)
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

      tree' _ v url  = do let v' = fromVerbatim v
                          res <- runOnce v' listTrees
                          (wd, m, trees) <- get3 pwd manager treeScripts
                          let cr = fromJust $ M.lookup v' trees
                              succ = crawlerFunction cr undefined
                              tree = fetchTree' m succ $ fromVerbatim url
                              doDownload = liftIO $ runExceptT $ extractBlobs tree
                                                                 >>= downloadFiles m wd
                          case res of Just _ -> return False
                                      Nothing -> doDownload >> return False

-- |Lists all comics.
listTrees :: Command (StateT AppState IO) (Either () Bool)
listTrees = makeCommand ":listTree" (`elem'` [":listTree"])
                        "Lists all available crawlers."
                        $ const (get1 treeScripts >>= mapM_ putStrLn . M.keys >> return False)

-- |Downloads a simple gallery.
gallery :: Command (StateT AppState IO) (Either () Bool)
gallery = makeCommand2 ":[g]allery" (`elem'` [":g",":gallery"])
                       "Downloads a list of numbered files."
                       urlAsk numAsk gallery'
   where
      urlAsk = predAsker "Enter URL of the first file: " undefined (const $ return True)

      numAsk = asker "Enter number of items: " err err (return . (>0))
         where err = "Expected positive integer!"

      gallery' _ url num = do let urls = pictureList' (fromVerbatim url) num
                              (wd, m) <- get2 pwd manager
                              liftIO $ runExceptT $ downloadFiles m wd urls
                              return False

-- |Printd a newline.
ln :: StateT AppState IO ()
ln = putStrLn ""






