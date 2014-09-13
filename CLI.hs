{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |CLI for the main program.
module CLI where

import Prelude hiding (putStrLn, succ, putStr, getLine)

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import Data.Char
import qualified Data.Map as M
import Data.Text (Text, append, strip)
import qualified Data.Text as T (map)
import System.FilePath.Posix.Text

import Galleries.Linear
import Galleries.Tree
import Fetch
import Fetch.Tree
import Galleries.Retrieval
import System.REPL

data AppState = AppState{wd::Text,
                         manager::Manager,
                         scriptDir::Text,
                         linearScripts::M.Map Text LinearCrawler,
                         treeScripts::M.Map Text VoidCrawler}

(=?=) :: Text -> Text -> Bool
(=?=) = curry $ uncurry (==) . (clean *** clean)

clean :: Text -> Text
clean = strip . T.map toLower

mainCLI :: AppState -> IO ()
mainCLI initState =
   do iterateUntilM (":e"=?=) (processCommand >=> const (liftIO prompt)) ""
         `runStateT`
          initState
      return ()

   where

      processCommand :: Text -> StateT AppState IO ()
      processCommand cmd
         | cmd =?= "" = return ()
         | cmd =?= ":comic" =
            do resp <- liftM clean $ liftIO $ prompt' "Enter comic name (type ':list' to show available): "
               if resp =?= ":list" then listComics
                                   else downloadComic resp
         | cmd =?= ":tree" =
            do resp <- liftM clean $ liftIO $ prompt' "Enter comic name:"
               downloadTree resp
         | cmd =?= ":gallery" =
            do resp <- liftM clean $ liftIO $ prompt' "Enter the URL of the first file: "
               (num::Int) <- askFor "Enter number of items: " "Expected Int!"
               let urls = pictureList' resp num
               (pwd,m) <- get2 wd manager
               liftIO $ runExceptT $ downloadFiles m pwd urls
               return ()
         | cmd =?= ":file" = do url <- liftM clean $ liftIO $ prompt' "Enter URL: "
                                (pwd, m) <- get2 wd manager
                                liftIO $ runExceptT $ downloadSave m pwd url
                                return ()
         | cmd =?= ":cd" = do putStrLn "Enter new download folder: "
                              x <- liftIO getLine
                              (pwd,st) <- get2 wd id
                              let p = normalise $ pwd </> x
                              if isValid p then put $ st{wd=p}
                                           else putErrLn "Couldn't parse path!"
         | cmd =?= ":pwd" = get >>= putStrLn . wd
         | cmd =?= ":help" = printHelp
         | otherwise = putErrLn
                       $ "Unknown command '" `append` cmd `append`
                         "'. Type :help for a list of available commands or"
                         `append` "':e' to exit."

version :: Text
version = "v1.0beta"

printHelp :: StateT AppState IO ()
printHelp = do
   putStrLn $ "Hephaesthos " `append` version
   ln
   putStrLn "CLI interface. Download files en masse."
   replicateM_ 2 ln
   cur <- get
   putStrLn $ "Current download folder: " `append` wd cur
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

ln :: StateT AppState IO ()
ln = putStrLn ""

listComics :: StateT AppState IO ()
listComics = get1 linearScripts >>= mapM_ putStrLn . M.keys

downloadComic :: Text -> StateT AppState IO ()
downloadComic c = do
   c' <- liftM (M.lookup c . linearScripts) get
   (pwd, m) <- get2 wd manager
   case c' of Nothing -> putStrLn "No comic by this name."
              Just v -> void (liftIO $ runExceptT
                              $ getLinearComic m v >>= downloadFiles m pwd)


downloadTree :: Text -> StateT AppState IO ()
downloadTree c = do
   (pwd, m, trees) <- get3 wd manager treeScripts
   case M.lookup c trees of
      Nothing -> putStrLn "No tree crawler by this name."
      Just v -> liftIO $ do url <- liftIO $ prompt' "Enter URL: "
                            let succ = crawlerFunction v undefined
                                tree = fetchTree' m succ url
                            runExceptT $ extractBlobs tree >>= downloadFiles m pwd
                            return ()




