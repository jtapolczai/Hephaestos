{-# LANGUAGE ScopedTypeVariables #-}

-- |CLI for the main program.
module CLI where

import Prelude hiding (putStrLn, succ)

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.State.Lazy hiding (state)
import Data.Char
import qualified Data.Map as M
import Data.String.Utils
import Data.Text (Text, unpack, pack)
import System.IO hiding (putStrLn)
import System.FilePath.Posix (combine, isValid, normalise)
import qualified System.IO as I (putStrLn)
import Text.Read (readMaybe)

import Galleries.Linear
import Galleries.Tree
import Fetch
import Fetch.Tree
import Galleries.Retrieval

data AppState = AppState{wd::FilePath,
                         manager::Manager,
                         scriptDir::String,
                         linearScripts::M.Map Text LinearCrawler,
                         treeScripts::M.Map Text DynTreeCrawler}

(=?=) :: String -> String -> Bool
(=?=) = curry $ uncurry (==) . (clean *** clean)

clean :: String -> String
clean = strip . map toLower

mainCLI :: AppState -> IO ()
mainCLI initState =
   do iterateUntilM (":e"=?=) (processCommand >=> const (liftIO prompt)) ""
         `runStateT`
          initState
      return ()

   where

      processCommand :: String -> StateT AppState IO ()
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
                              let p = normalise $ combine pwd x
                              if isValid p then put $ st{wd=p}
                                           else putErrLn "Couldn't parse path!"
         | cmd =?= ":pwd" = get >>= putStrLn . wd
         | cmd =?= ":help" = printHelp
         | otherwise = putErrLn
                       $ "Unknown command '" ++ cmd ++ "'. Type :help for " ++
                         "a list of available commands or ':e' to exit."

prompt :: IO String
prompt = prompt' "> "

prompt' :: String -> IO String
prompt' s = putStr s >> hFlush stdout >> getLine

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
   putStrLn ":tree    -- Download a tree of files."
   putStrLn ":gallery -- Downloads a simple gallery (a list of elements"
   putStrLn "            on a single page."
   putStrLn ":file    -- Downloads a single file."

ln :: StateT AppState IO ()
ln = putStrLn ""

listComics :: StateT AppState IO ()
listComics = get1 linearScripts >>= mapM_ (putStrLn . unpack) . M.keys

downloadComic :: String -> StateT AppState IO ()
downloadComic c = do
   c' <- liftM (M.lookup (pack c) . linearScripts) get
   (pwd, m) <- get2 wd manager
   case c' of Nothing -> putStrLn "No comic by this name."
              Just v -> void (liftIO $ runExceptT
                              $ getLinearComic m v >>= downloadFiles m pwd)


downloadTree :: String -> StateT AppState IO ()
downloadTree c = do
   (pwd, m, trees) <- get3 wd manager treeScripts
   case M.lookup (pack c) trees of
      Nothing -> putStrLn "No tree crawler by this name."
      Just v -> liftIO $ do url <- liftIO $ prompt' "Enter URL: "
                            treeF <- runCrawler v
                            runExceptT $ flattenTree (treeF m url) >>= downloadFiles m pwd
                            return ()


putErrLn :: String -> StateT AppState IO ()
putErrLn s = liftIO $ hPutStrLn stderr s

askFor :: Read a => String -> String -> StateT AppState IO a
askFor s err = do y <- liftIO $ prompt' s
                  let x = readMaybe y
                  case x of Nothing -> putErrLn s >> askFor s err
                            (Just x') -> return x'

get1 :: Monad m => (s -> a) -> StateT s m a
get1 f1 = liftM f1 get

get2 :: Monad m => (s -> a) -> (s -> b) -> StateT s m (a,b)
get2 f1 f2 = liftM (f1 &&& f2) get

get3 :: Monad m => (s -> a) -> (s -> b) -> (s -> c) -> StateT s m (a,b,c)
get3 f1 f2 f3 = liftM (\x -> (f1 x, f2 x, f3 x)) get

get4 :: Monad m => (s -> a) -> (s -> b) -> (s -> c) -> (s -> d) -> StateT s m (a,b,c,d)
get4 f1 f2 f3 f4 = liftM (\x -> (f1 x, f2 x, f3 x, f4 x)) get