-- |Formatted and colored output to the console.
module Crawling.Hephaestos.CLI.Format (
   format,
   error,
   report,
   input,
   emphasize,
   cliAction,) where

import Prelude hiding (error)

import Control.Concurrent.STM.Utils
import Control.Monad.IO.Class
import System.Console.ANSI
import System.IO.Unsafe (unsafePerformIO)

-- |Formats the console before executing an action. Afterwards, the
--  console is re-formatted with 'Reset'.
format :: MonadIO m => [SGR] -> m a -> m a
format sgr f = liftIO (setSGR sgr) >> f >>= \x -> liftIO (setSGR [Reset]) >> return x

-- |Formats the console to print an error.
error :: MonadIO m => m a -> m a
error = format [SetColor Foreground Vivid Red]

-- |Formats the console to print a report of a finished activity (i.e.
--  a finished downloading process).
report :: MonadIO m => m a -> m a
report = format [SetColor Foreground Vivid Cyan]

-- |Formats the console to print a request for user input.
input :: MonadIO m => m a -> m a
input = format [SetColor Foreground Vivid White]

-- |Formats the console to print a emphasized text.
emphasize :: MonadIO m => m a -> m a
emphasize = format [SetColor Foreground Vivid White,
                    SetConsoleIntensity BoldIntensity]

-- |Global mutex for using the CLI.
cliMutex :: TaskLimit
cliMutex = unsafePerformIO $ newTaskLimitIO (Just 1)

-- |Performs an action using 'cliMutex'. Use this to ensure that output
--  isn't interleaved in a multi-threaded environment.
--
-- __Note__: nesting calls of 'cliAction' leads to deadlocks. This is especially
--           the case when you call a function that uses 'cliAction' from
--           another function that uses it.
cliAction :: MonadIO m => IO a -> m a
cliAction = withTaskLimit cliMutex . liftIO
