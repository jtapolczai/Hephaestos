{-# LANGUAGE ScopedTypeVariables #-}

module Hephaestos where

import Prelude hiding (putStrLn)

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy
import Data.Char
import qualified Data.Map as M
import Data.String.Utils
import Data.Text (Text, unpack, pack)
import System.Directory (getCurrentDirectory)
import System.IO hiding (putStrLn)
import System.FilePath.Posix (combine, isValid, normalise)
import qualified System.IO as I (putStrLn)
import Text.Read (readMaybe)

import Comics.LinearComic
import Comics.List
import Fetch
import Galleries.Retrieval

data AppState = AppState{wd::FilePath,
                         scriptDir::String,
                         scripts::M.Map Text LinearComic}

(=?=) :: String -> String -> Bool
(=?=) = curry $ uncurry (==) . (clean *** clean)

clean :: String -> String
clean = strip . map toLower

main :: IO ()
main = runExceptT (initState >>= liftIO . runStateT main') >>= printErr
   where
      printErr (Right _) = return ()
      printErr (Left e) = mapM_ print e

      initState :: ErrorIO AppState
      initState = do dlf <- catchIO "File" FileError downloadsFolder
                     cur <- catchIO "File" FileError getCurrentDirectory
                     let scd = cur `combine` "scripts/"
                     sc <- comics scd
                     return AppState{wd=dlf,scriptDir=scd,scripts=sc}

      main' :: StateT AppState IO String
      main' = iterateUntilM (":e"=?=) (processCommand >=> const prompt) ""

      processCommand :: String -> StateT AppState IO ()
      processCommand cmd
         | cmd =?= "" = return ()
         | cmd =?= ":comic" =
            do resp <- liftM clean $ prompt' "Enter comic name (type ':list' to show available): "
               if resp =?= ":list" then listComics >> processCommand ":comic"
                                   else downloadComic resp
         | cmd =?= ":gallery" =
            do resp <- liftM clean $ prompt' "Enter the URL of the first file: "
               (num::Int) <- askFor "Enter number of items: " "Expected Int!"
               let urls = pictureList' resp num
               pwd <- liftM wd get
               liftIO $ runExceptT $ withManager (`downloadFiles` pwd) Nothing urls
               return ()
         | cmd =?= ":file" = do url <- liftM clean $ prompt' "Enter URL: "
                                pwd <- liftM wd get
                                liftIO $ runExceptT $ withManager (`downloadSave` pwd) Nothing url
                                return ()
         | cmd =?= ":cd" = do putStrLn "Enter new download folder: "
                              x <- liftIO getLine
                              pwd <- liftM wd get
                              st <- get
                              let p = normalise $ combine pwd x
                              if isValid p then put $ st{wd=p}
                                           else putErrLn "Couldn't parse path!"
         | cmd =?= ":pwd" = get >>= putStrLn . wd
         | cmd =?= ":help" = printHelp
         | otherwise = putErrLn
                       $ "Unknown command '" ++ cmd ++ "'. Type :help for " ++
                         "a list of available commands or ':e' to exit."

prompt :: StateT AppState IO String
prompt = prompt' "> "

prompt' :: String -> StateT AppState IO String
prompt' s = liftIO $ putStr s >> hFlush stdout >> getLine

putStrLn :: String -> StateT AppState IO ()
putStrLn s = liftIO $ I.putStrLn s

version :: String
version = "v1.0beta"

printHelp :: StateT AppState IO ()
printHelp = do
   putStrLn $ "Hephaesthos " ++ version
   ln
   putStrLn "CLI interface. Download files en masse."
   replicateM_ 2 ln
   cur <- get
   putStrLn $ "Current download folder: " ++ wd cur
   ln
   putStrLn "Available commands:"
   putStrLn ":help    -- Prints this message."
   putStrLn ":e       -- Exits the program."
   putStrLn ":pwd     -- \"Print working directory\"; shows the current download folder."
   putStrLn ":cd      -- Changes the current download folder."
   putStrLn ":comic   -- Downloads a webcomic."
   putStrLn ":gallery -- Downloads a simple gallery (a list of elements"
   putStrLn "            on a single page."
   putStrLn ":file    -- Downloads a single file."

ln :: StateT AppState IO ()
ln = putStrLn ""

listComics :: StateT AppState IO ()
listComics = get >>= mapM_ (putStrLn . unpack) . M.keys . scripts

downloadComic :: String -> StateT AppState IO ()
downloadComic c = do
   c' <- liftM (M.lookup (pack c) . scripts) get
   pwd <- liftM wd get
   case c' of Nothing -> putStrLn "No comic by this name."
              Just v -> void (liftIO $ runExceptT $ withManager
                              (\m _ -> do comic <- getLinearComic m v
                                          downloadFiles m pwd comic) Nothing undefined)

putErrLn :: String -> StateT AppState IO ()
putErrLn s = liftIO $ hPutStrLn stderr s

askFor :: Read a => String -> String -> StateT AppState IO a
askFor s err = do y <- prompt' s
                  let x = readMaybe y
                  case x of Nothing -> putErrLn s >> askFor s err
                            (Just x') -> return x'
