-- |Formatted and colored output to the console.
module Crawling.Hephaestos.CLI.Color where

import Control.Monad.Except
import Data.Functor.Monadic
import System.Console.ANSI

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
