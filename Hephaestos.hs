module Hephaesthos where

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy
import Data.Char
import Data.String.Utils
import System.IO

import Fetch
import Fetch.Iterating
import Fetch.Job
import Fetch.Mapping

import Comics
import Galleries.Simple
import Galleries.Retrieval

data AppState = AppState{wd::FilePath}

(=?=) :: String -> String -> Bool
(=?=) = curry $ uncurry (==) . (clean *** clean)

clean :: String -> String
clean = strip . map toLower

main :: IO ()
main = void $ liftM AppState downloadsFolder >>= runStateT main'
   where
      main' :: StateT AppState IO String
      main' = iterateUntilM (":e"=?=) (processCommand >=> const prompt) ""

      processCommand :: String -> StateT AppState IO ()
      processCommand cmd
         | cmd =?= "" = return ()
         | cmd =?= ":comic" =
            do liftIO $ putStrLn "Enter comic name (type ':list' to show available):"
               resp <- liftIO $ liftM clean getLine
               if resp =?= ":list" then liftIO listComics >> processCommand ":comic"
                                   else downloadComic resp
         | cmd =?= ":gallery" =
            do liftIO $ putStrLn "Enter the URL of the first file:"
               resp <- liftIO $ liftM clean getLine
               downloadSimpleGallery resp
         | cmd =?= ":tree" = undefined
         | cmd =?= ":file" = undefined
         | cmd =?= ":cd" = undefined
         | cmd =?= ":pwd" = get >>= putStrLn' . wd
         | cmd =?= ":help" = printHelp
         | otherwise = putErrLn
                       $ "Unknown command '" ++ cmd ++ "'. Type :help for " ++
                         "a list of available commands or ':e' to exit."

prompt :: StateT AppState IO String
prompt = liftIO $ putStr "> " >> hFlush stdout >> getLine

putStrLn' :: String -> StateT AppState IO ()
putStrLn' s = liftIO $ putStrLn s

version :: String
version = "v1.0beta"

printHelp :: StateT AppState IO ()
printHelp = do
   putStrLn' $ "Hephaesthos " ++ version
   liftIO ln
   putStrLn' "CLI interface. Download files en masse."
   liftIO $ replicateM 2 ln
   cur <- get
   putStrLn' $ "Current download folder: " ++ (wd cur)
   liftIO ln
   putStrLn' "Available commands:"
   putStrLn' ":help    -- Prints this message."
   putStrLn' ":e       -- Exits the program."
   putStrLn' ":pwd     -- \"Print working directory\"; shows the current download folder."
   putStrLn' ":cd      -- Changes the current download folder."
   putStrLn' ":comic   -- Downloads a webcomic."
   putStrLn' ":gallery -- Downloads a simple gallery (a list of elements"
   putStrLn' "            on a single page."
   putStrLn' ":file    -- Downloads a single file."
   putStrLn' ":tree    -- Downloads a complex collection of branching content."

ln :: IO ()
ln = putStrLn ""

listComics = undefined
downloadComic = undefined
downloadSimpleGallery = undefined

putErrLn :: String -> StateT AppState IO ()
putErrLn s = liftIO $ hPutStrLn stderr s