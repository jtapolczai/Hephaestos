{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |Provides Commands for REPLs. Commands take care of input
--  and parameter-handling, and allow parameters to be supplied
--  in the same line as the command's name (e.g. ":cmd param1 param2" on stdin).
--  Provided parameters can be parsed and checked (say, against databases)
--  before they are passed to the actual command function.
--  They are relatively large units of abstraction, but they allow the easy
--  creation of relatively sophisticated command loops, and have the advantage
--  that one doesn't need to fiddle around with input handling in the middle
--  of the actual command code.
module System.REPL.Command (
   -- *Command dispatch
   -- |Using the 'Command' class is not necessary, but it makes dealing with
   --  user input considerably easier. When a command is run with a line of
   --  input, it automatically segments it by whitespace, tries to interpret
   --  each part as one of its arguments and passes them to the actual command
   --  function. If any arguments haven't been supplies, it asks for them on
   --  stdin. If too many arguments have been supplied, or if any argument'
   --  parsing returns an error, the command is aborted.
   --
   --  Example:
   --
   --  > cd = makeCommand1 ...
   --
   --  >>> :cd ../
   --  Directory changed!
   --  >>> :cd
   --  Enter new directory:
   --  >>> ../
   --  Directory changed!
   Command(..),
   commandInfo,
   runOnce,
   commandDispatch,
   summarizeCommands,
   -- ** Making commands.
   makeCommand,
   makeCommand1,
   makeCommand2,
   makeCommand3,
   makeCommand4,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, unwords, words, (!!), (++))
import qualified Prelude as P

import Control.Arrow (right, (+++), left)
import Control.Monad
import Control.Monad.State
import Data.Char (isSpace)
import Data.Either (lefts)
import Data.Either.Optional
import Data.Functor ((<$>))
import qualified Data.List as L
import Data.Maybe (listToMaybe, fromJust, isNothing)
import Data.Ord
import Data.Text
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Crawling.Hephaestos.Helper.Functor
import Crawling.Hephaestos.Helper.String ((++), padRight', show')
import System.IO hiding (putStrLn, putStr, getLine)
import Text.Read (readMaybe)

import System.REPL
import System.REPL.Command.Helper

-- |A REPL command, possibly with parameters.
data Command m a = Command{
                  -- |The short name of the command. Purely informative.
                  commandName :: Text,
                  -- |Returns whether a string matches
                  --  a command name. The simplest form is
                  --  @s==@ for some string s, but more liberal
                  --  matchings are possible.
                  commandTest :: Text -> Bool,
                  -- |A description of the command.
                  commandDesc :: Text,
                  -- |The number of parameters, if fixed.
                  numParameters :: Maybe Int,
                  -- |Runs the command with the input text as parameter.
                  runCommand :: Text -> m a}

-- |Prints information (the command name, description and, if given,
--  the number of parameters) about a command to the console.
commandInfo :: MonadIO m => Command m a -> m ()
commandInfo c = do putStr $ commandName c
                   putStrLn $ maybe "" ((" Parameters: " P.++) . show) (numParameters c)
                   putStrLn $ commandDesc c

-- |Splits and cleans the input of a command.
--  Be aware that this is not a security-related function!
--  Defined as @map strip . words . strip@.
sanitize :: Text -> [Text]
sanitize = Prelude.map strip . words . strip

-- |Takes a line of text and a command.
--  If the text matches the given command's 'commandTest',
--  the command is run with it. If not, 'Nothing' is returned.
runOnce :: MonadIO m => Text -> Command m a -> m (Maybe a)
runOnce l c = if commandTest c l then liftM Just (runCommand c l)
                                 else return Nothing

-- |Prints an error message if an unexpected number of parameters have been
--  supplied.
paramErr :: MonadIO m
         => Text -- ^The command name.
         -> Text -- ^The given input.
         -> Int  -- ^The expected number of parameters.
         -> m ()
paramErr c inp 0 = putErrLn msg
   where tail' [] = []
         tail' (x:xs) = xs

         msg :: Text
         msg = "Parameters " ++ unwords (tail' $ sanitize inp)
               ++ " given to " ++ c ++ ". " ++ c ++ " takes no parameters."


paramErr c inp n = putErrLn $ l ++ " parameters given to " ++ c
                              ++ ". " ++ c ++ " takes "
                              ++ n' ++ " parameters."
   where l = pack . show . (\x -> x-1) . P.length . sanitize $ inp
         n' = pack.show $ n


-- |Checks the number of parameters before executing a monadic function.
checkParams :: MonadIO m
            => Text -- ^The command name.
            -> Text -- ^The raw input (including the command name).
            -> Int -- ^The expected number of parameters, excluding the command's name itself.
            -> ([Text] -> m (Either (AskFailure e) a)) -- ^The command.
            -> m (Either (AskFailure e) a) -- ^Result. If too many parameters were
                                           --  passed, this will be a 'ParamNumFailure'.
checkParams n inp num m =
   if P.length (sanitize inp) > (num + 1) then paramErr n inp num
                                               >> return (Left ParamNumFailure)
   else m (sanitize inp)

-- |Throws a user error if a command was called on an empty command string
--  (which is illegal, as the command string at least has to contain the command's
--  non-empty name).
noStringErr :: a
noStringErr = error "Called command with empty command string. Expected at least the command name."

-- |Creates a command without parameters.
makeCommand :: (MonadIO m)
            => Text -- ^Command name.
            -> (Text -> Bool) -- ^Command test.
            -> Text -- ^Command description.
            -> (Text -> m a) -- ^The actual command.
            -> Command m (Either (AskFailure e) a)
makeCommand n t d f = Command n t d (Just 0) (\inp -> checkParams n inp 0 c)
   where
      c inp = liftEM f (toEither noStringErr (listToMaybe inp))

-- |Creates a command with one parameter.
makeCommand1 :: (MonadIO m, Functor m, Read a, Show e)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> (Text -> a -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand1 n t d p1 f = Command n t d (Just 1) (\inp -> checkParams n inp 1 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 printError [voidR x1]
                 liftEM2 f li x1

-- |Creates a command with two parameters.
makeCommand2 :: (MonadIO m, Functor m, Show e, Read a, Read b)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> Asker m b e -- ^'Asker' for the second perameter.
             -> (Text -> a -> b -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand2 n t d p1 p2 f = Command n t d (Just 2) (\inp -> checkParams n inp 2 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 printError [voidR x1, voidR x2]
                 liftEM3 f li x1 x2

-- |Creates a command with three parameters.
makeCommand3 :: (MonadIO m, Functor m, Show e, Read a, Read b, Read c)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> Asker m b e -- ^'Asker' for the second perameter.
             -> Asker m c e -- ^'Asker' for the third parameter.
             -> (Text -> a -> b -> c -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand3 n t d p1 p2 p3 f = Command n t d (Just 3) (\inp -> checkParams n inp 3 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 x3 <- onSucc x2 $ ask p3 (inp !! 3)
                 printError [voidR x1, voidR x2, voidR x3]
                 liftEM4 f li x1 x2 x3

-- |Creates a command with four parameters.
makeCommand4 :: (MonadIO m, Functor m, Show e, Read a, Read b, Read c, Read d)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> Asker m b e -- ^'Asker' for the second perameter.
             -> Asker m c e -- ^'Asker' for the third parameter.
             -> Asker m d e -- ^'Asker' for the fourth parameter.
             -> (Text -> a -> b -> c -> d -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand4 n t d p1 p2 p3 p4 f = Command n t d (Just 4) (\inp -> checkParams n inp 4 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 x3 <- onSucc x2 $ ask p3 (inp !! 3)
                 x4 <- onSucc x3 $ ask p4 (inp !! 4)
                 printError [voidR x1, voidR x2, voidR x3, void x4]
                 liftEM5 f li x1 x2 x3 x4


-- |Prints the first AskFailure in a list of values, if there is one.
printError :: (MonadIO m, Show e) => [Either (AskFailure e) b] -> m ()
printError xs = case lefts xs of []    -> return ()
                                 (x:_) -> putErrLn $ T.pack $ show $ failureText x


-- |Takes an input and tries to run it against a list of commands,
--  trying the out in sequence. The first command whose 'commandTest'
--  returns True is executed. If none of the commands match,
--  @m (Left NothingFoundFailure)@ is returned.
commandDispatch :: (Monad m, Functor m)
                => Text -- ^The user's input.
                -> [Command m (Either (AskFailure e) z)] -- ^The command library.
                -> m (Either (AskFailure e) z)
commandDispatch input cs =
   if P.null input' || noMatch then return $ Left NothingFoundFailure
   else runCommand (fromJust first) input
   where
      input' = sanitize input
      noMatch = isNothing first
      first = listToMaybe $ P.dropWhile (not . flip commandTest (P.head input')) cs


-- |Prints out a list of command names, with their descriptions.
summarizeCommands :: MonadIO m
                  => [Command m2 (Either a b)]
                  -> m ()
summarizeCommands [] = return ()
summarizeCommands xs@(_:_) = mapM_ (\c -> prName c >> prDesc c) xs
   where
      maxLen :: Int
      maxLen = T.length
               $ commandName
               $ L.minimumBy (comparing $ (* (-1)) . T.length . commandName) xs
      prName = putStr . padRight' ' ' maxLen . commandName
      prDesc = putStrLn . (" - " ++) . commandDesc
